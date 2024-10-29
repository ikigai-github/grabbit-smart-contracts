module Vulcan.Onchain.Auction.Validator.Private (
  privateAuctionValidator,
  pPrivateAuctionValidator,
  mkPrivateAuctionValidator,
) where

import Plutarch.Api.V1 qualified as V1
import Plutarch.Api.V1.Value qualified as V
import Plutarch.Api.V2 (
  PScriptContext,
  PValidator,
 )

import Plutarch.Extra.Interval (pafter)
import Plutarch.Monadic qualified as P

import PlutusLedgerApi.V2 (CurrencySymbol, Validator)

import Vulcan.Onchain.Auction.Validator.Common (
  makeCommon,
  pBidAct,
  pBuyNowAct,
  pCancelAct,
  pCloseExtendAct,
  pExtendAct,
  pRefundAct,
  pResolveAuctionAct,
 )
import Vulcan.Onchain.Auction.Validator.Helpers (
  PValidatorCommon (fromValidator, minAda, mint, outs, range, refAtVal, sigs, toValidator),
  correctPaid,
 )
import Vulcan.SpecialUTxO.Types (
  NamedToken (pnameToken),
  pnodeKeyToken,
 )
import Vulcan.SpecialUTxO.Utils (
  auctionDatumFromInputs,
  parseBidOutput,
  registrationInputUtxoDatum,
 )

import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Types.Auction (
  PBidEscrow,
  PBidStatus (PNoBid),
  PPrivAuctionRedeemer (
    PBidAct,
    PBuyNowAct,
    PCancelAct,
    PCloseExtensionsAct,
    PExtendAct,
    PIssuePaddleAct,
    PRefundAct,
    PResolveAct,
    PUnregisterAct
  ),
  PRegistrationEscrow,
 )
import Vulcan.Utils (
  passert,
  pfieldh,
  ptxSignedBy,
  pverifyDataC,
  toPlutarchConfig,
  tryPkFromAddress,
  wrapValidator,
 )

mkPrivateAuctionValidator ::
  Vulcan.Config ->
  CurrencySymbol ->
  ClosedTerm
    ( PData
        :--> PPrivAuctionRedeemer
        :--> PScriptContext
        :--> PUnit
    )
mkPrivateAuctionValidator cfg marketCS = plam $ \_ redeemer ctx' -> P.do
  (common, market) <-
    runTermCont $ makeCommon marketCS ctx'

  ---------------------------
  -- Validation:

  pmatch redeemer $ \case
    PCancelAct act' -> P.do
      act <- pletFields @'["cs", "reason"] act'
      pCancelAct cfg common # pfromData act.reason # pfromData act.cs
    PUnregisterAct (pfieldh @"cs" -> cs) ->
      pUnregisterAct cfg common # pfromData cs
    PIssuePaddleAct (pfieldh @"cs" -> cs) ->
      pIssuePaddleAct cfg common # pfromData cs
    PBidAct (pfieldh @"cs" -> cs) ->
      pBidAct cfg common # pfromData cs # market ---
    PExtendAct (pfieldh @"cs" -> cs) -> pExtendAct cfg common # pfromData cs
    PCloseExtensionsAct (pfieldh @"cs" -> cs) -> pCloseExtendAct cfg common # pfromData cs
    PBuyNowAct (pfieldh @"cs" -> cs) ->
      pBuyNowAct cfg common # pfromData cs # market
    PRefundAct n -> P.do
      act <- pletFields @'["cs", "reason"] n
      pRefundAct cfg common # pfromData act.reason # pfromData act.cs
    PResolveAct (pfieldh @"cs" -> cs) ->
      pResolveAuctionAct cfg common # pfromData cs # market

---------------------------
-- Unique Redeemers:
---------------------------
pUnregisterAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (V1.PCurrencySymbol :--> PUnit)
pUnregisterAct cfg common = plam $ \cs -> P.do
  -- Inputs
  registrationUtxo <-
    pletFields @'["datum", "extraValue"] $
      -- RegistrationEscrow UTxO to destroy
      registrationInputUtxoDatum # common.minAda # cs # common.fromValidator
  registrant <- plet $ pfield @"registrant" # registrationUtxo.datum
  let regPK = tryPkFromAddress # registrant

      isReturned =
        correctPaid
          # registrant
          #$ registrationUtxo.extraValue <> common.minAda
      -- Minting:
      -- from RegistrationEscrow UTxO
      registrationToken =
        V.psingleton # cs # pnameToken @PRegistrationEscrow # (-1)
  ---------------------------
  -- Checks:

  passert cfg "Only RegistrationEscrow must be spend from AuctionValidator" $
    plength # common.fromValidator #== 1

  passert cfg "Tx must be signed by registrant" $
    ptxSignedBy # common.sigs # regPK

  passert cfg "RegitrationEscrow ADA must be returned" $
    pany # isReturned # common.outs

  passert cfg "Only the RegistrationEscrow token may be burnt" $
    common.mint #== registrationToken

  pconstant ()

--------------------------------

pIssuePaddleAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (V1.PCurrencySymbol :--> PUnit)
pIssuePaddleAct cfg common = plam $ \cs -> P.do
  -- Reference Inputs:
  auctionEscrow <-
    pletFields @'["terms", "nodeCS"] $
      auctionDatumFromInputs cfg # cs # common.refAtVal
  terms <-
    pletFields
      @'["lot", "bought", "auctionInfo", "time"]
      auctionEscrow.terms
  let nodeCS = pfromData auctionEscrow.nodeCS
      seller = pfield @"seller" # terms.auctionInfo
      close = pfromData $ pfield @"close" # terms.time

  -- Inputs
  registrationIn <-
    plet $
      registrationInputUtxoDatum # common.minAda # cs # common.fromValidator

  registration <- pletFields @'["datum", "extraValue", "staking"] registrationIn
  let registrant = pfield @"registrant" # registration.datum

  -- Outputs:
  bidOut <-
    pletFields @'["datum", "extraValue", "staking"] $
      parseBidOutput # common.minAda # cs # common.toValidator
  bidDatum <- pletFields @'["bidder", "status"] bidOut.datum
  let bidder = tryPkFromAddress # bidDatum.bidder

  let status = pcon $ PNoBid pdnil
      -- Minting:
      -- from destroyed RegistrationEscrow UTxO
      registrationToken =
        V.psingleton # cs # pnameToken @PRegistrationEscrow # (-1)
      -- from new BidEscrow UTxO
      bidToken = V.psingleton # cs # pnameToken @PBidEscrow # 1
      -- from new SetNode UTxO that corresponds to a new bid
      bidNodeToken = pnodeKeyToken # nodeCS # bidder # 1

  -----------------------------
  -- Checks:

  passert cfg "Only RegistrationEscrow must be spend from AuctionValidator" $
    plength # common.fromValidator #== 1

  passert cfg "Tx must be signed by seller" $
    ptxSignedBy # common.sigs #$ tryPkFromAddress # seller

  passert cfg "Incorrect address" $
    bidDatum.bidder #== registrant

  passert cfg "No bid allowed" $
    pfromData bidDatum.status #== status

  passert cfg "Registration value not transferred" $
    pfromData bidOut.extraValue #== pfromData registration.extraValue

  passert cfg "Auction has already ended" $
    pafter # close # common.range

  passert cfg "Incorrect mint amounts" $
    common.mint #== bidToken <> registrationToken <> bidNodeToken

  pconstant ()

---------------------------

privateAuctionValidator :: Vulcan.Config -> CurrencySymbol -> Validator
privateAuctionValidator config cs =
  wrapValidator (toPlutarchConfig config) $
    mkPrivateAuctionValidator config cs

pPrivateAuctionValidator :: Vulcan.Config -> CurrencySymbol -> ClosedTerm PValidator
pPrivateAuctionValidator cfg cs = plam $ \datm redm' ctx -> unTermCont $ do
  redm <- pverifyDataC @PPrivAuctionRedeemer redm'
  pure . popaque $
    mkPrivateAuctionValidator cfg cs
      # datm
      # pfromData redm
      # ctx
