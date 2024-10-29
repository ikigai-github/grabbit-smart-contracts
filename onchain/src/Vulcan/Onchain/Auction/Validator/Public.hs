module Vulcan.Onchain.Auction.Validator.Public (
  mkPublicAuctionValidator,
  pPublicAuctionValidator,
  publicAuctionValidator,
) where

import Plutarch.Api.V2 (PScriptContext, PValidator)

import Plutarch.Monadic qualified as P

import PlutusLedgerApi.V2 (CurrencySymbol, Validator)

import Plutarch.Api.V1 (PAddress, PCurrencySymbol)
import Plutarch.Api.V1.Value qualified as V
import Vulcan.Common.Types.Instances qualified as Vulcan
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
import Vulcan.Onchain.Auction.Validator.Helpers (PValidatorCommon (..), correctResolvedPaymentsNoEnroll)
import Vulcan.SpecialUTxO.Types (boughtTN)
import Vulcan.SpecialUTxO.Utils (checkBoughtUtxoValue, pisAuction)
import Vulcan.Types.Auction (
  PAuctionEscrow,
  PPubAuctionRedeemer (
    PPubBidAct,
    PPubBuyNowAct,
    PPubBuyNowNoEnrollAct,
    PPubCancelAct,
    PPubCloseExtensionsAct,
    PPubExtendAct,
    PPubRefundAct,
    PPubResolveAct
  ),
 )
import Vulcan.Types.Market (PMarketTerms (..))
import Vulcan.Utils (
  padaTolovelace,
  passert,
  passumeWellFormedInlineDatum,
  pfieldh,
  pfindWithRest,
  ptryFind,
  pverifyDataC,
  toPlutarchConfig,
  wrapValidator,
 )

pBuyNowNoEnrollAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (PAsData PAddress) ->
  Term s (PCurrencySymbol :--> PMarketTerms :--> PUnit)
pBuyNowNoEnrollAct cfg common receiver = plam $ \cs market -> P.do
  --------------------------------
  -- Inputs:
  PPair auctionIn rest <-
    pmatch $ pfindWithRest # (pisAuction # cs) # common.fromValidator

  -- Extracting data from auction:
  auctionInDatum <- plet $ pfield @"datum" # auctionIn

  auctionEscrow <-
    pletFields @["terms", "nodeCS"] $
      passumeWellFormedInlineDatum @PAuctionEscrow # auctionInDatum

  let buyNowPrice =
        padaTolovelace #$ pfromData $
          pfield @"buyNowPrice"
            #$ pfield @"bidInfo" # auctionEscrow.terms
      nodeCS = auctionEscrow.nodeCS

  --------------------------------
  -- Outputs:
  auctionOut <- plet $ ptryFind # (pisAuction # cs) # common.toValidator

  --------------------------------
  -- Mints:
  -- to AuctionEscrow UTxO:
  let boughtToken = V.psingleton # cs # boughtTN # 1

  -------------------------------
  -- Checks:

  passert cfg "Auction must not already be bought" $
    V.pvalueOf # (pfield @"value" # auctionIn) # cs # boughtTN #== 0

  passert cfg "Only AuctionEscrow should be spent from AuctionValidator" $
    pnull # rest

  passert cfg "Incorrect mint amounts" $
    common.mint #== boughtToken

  passert cfg "Incorrect bought auction value" $
    checkBoughtUtxoValue # common.minAda # cs # nodeCS # auctionOut

  passert cfg "Datum must not be altered" $
    auctionInDatum #== (pfield @"datum" # auctionOut)

  correctResolvedPaymentsNoEnroll cfg
    # buyNowPrice
    # receiver
    # market
    # auctionEscrow.terms
    # common.outs

mkPublicAuctionValidator ::
  Vulcan.Config ->
  CurrencySymbol ->
  ClosedTerm
    ( PData
        :--> PPubAuctionRedeemer
        :--> PScriptContext
        :--> PUnit
    )
mkPublicAuctionValidator cfg marketCS = plam $ \_ redeemer ctx' -> P.do
  (common, market) <-
    runTermCont $ makeCommon marketCS ctx'

  pmatch redeemer $ \case
    PPubCancelAct act' -> P.do
      act <- pletFields @'["cs", "reason"] act'
      pCancelAct cfg common # pfromData act.reason # pfromData act.cs
    PPubBidAct (pfieldh @"cs" -> cs) ->
      pBidAct cfg common # pfromData cs # market
    PPubExtendAct (pfieldh @"cs" -> cs) -> pExtendAct cfg common # pfromData cs
    PPubCloseExtensionsAct (pfieldh @"cs" -> cs) -> pCloseExtendAct cfg common # pfromData cs
    PPubBuyNowAct (pfieldh @"cs" -> cs) ->
      pBuyNowAct cfg common # pfromData cs # market
    PPubBuyNowNoEnrollAct act' -> P.do
      act <- pletFields @'["cs", "receiver"] act'
      pBuyNowNoEnrollAct cfg common act.receiver
        # pfromData act.cs
        # market
    PPubRefundAct n -> P.do
      act <- pletFields @'["cs", "reason"] n
      pRefundAct cfg common
        # pfromData act.reason
        # pfromData act.cs
    PPubResolveAct (pfieldh @"cs" -> cs) ->
      pResolveAuctionAct cfg common # pfromData cs # market

-- Public AuctionValidator redeemers are a subset of Private AuctionValidator
-- with the exception of PPubBuyNowNoEnrollAct which is unique to public auctions.

--------------------------------------

publicAuctionValidator :: Vulcan.Config -> CurrencySymbol -> Validator
publicAuctionValidator config cs =
  wrapValidator (toPlutarchConfig config) $ mkPublicAuctionValidator config cs

pPublicAuctionValidator :: Vulcan.Config -> CurrencySymbol -> Term s PValidator
pPublicAuctionValidator cfg cs = plam $ \datm redm' ctx -> unTermCont $ do
  redm <- pverifyDataC @PPubAuctionRedeemer redm'
  pure . popaque $
    mkPublicAuctionValidator cfg cs
      # datm
      # pfromData redm
      # ctx
