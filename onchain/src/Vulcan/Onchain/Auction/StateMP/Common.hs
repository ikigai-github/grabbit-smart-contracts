module Vulcan.Onchain.Auction.StateMP.Common (
  pAnnounceAuction,
  plifeCycle,
  PStateCommon (..),
  makeCommon,
) where

import Plutarch.Api.V1 (
  AmountGuarantees (NonZero, Positive),
  KeyGuarantees (Sorted),
  PCurrencySymbol,
  PPOSIXTimeRange,
  PScriptPurpose (PMinting),
  PValidatorHash,
  PValue,
 )
import Plutarch.Api.V1.Value qualified as V
import Plutarch.Api.V2 (
  PPubKeyHash,
  PScriptContext,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P

import Data.Text qualified as T
import Plutarch.Api.V1.Value (pnormalize)
import PlutusLedgerApi.V1 (CurrencySymbol, ValidatorHash)
import Vulcan.Common.Constants qualified as Constants (maxAmountOfAssetsInLot)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.SpecialUTxO.Types (
  NamedToken (pnameToken),
  pcorrNodeTN,
  poriginNodeTN,
 )
import Vulcan.SpecialUTxO.Utils (
  marketInputUtxoDatum,
  parseAuctionFromOutputs,
 )
import Vulcan.Types.Auction (PAuctionEscrow)
import Vulcan.Types.Market (PMarketTerms)
import Vulcan.Utils (
  hasValueAndRef,
  padaTolovelace,
  passert,
  patScriptAddress,
  pfield0,
  pfromScriptAddress,
  pgetCS,
  plovelace,
  psumData,
  ptryFirstJust,
  ptxSignedBy,
  tryPkFromAddress,
 )
import Vulcan.Utils.Value (pamountOfAssets)

-- | Collect information needed for validation:
makeCommon ::
  forall {r :: PType} {s :: S}.
  ValidatorHash ->
  CurrencySymbol ->
  Term s PCurrencySymbol ->
  Term s PScriptContext ->
  TermCont @r
    s
    ( PStateCommon s
    , Term s (PAsData PPOSIXTimeRange)
    , Term s PMarketTerms
    , Term s (PBuiltinList PTxOut)
    )
makeCommon auctionValHash marketCS nodeCS ctx' = do
  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
  txInfo <-
    tcont $
      pletFields
        @'["inputs", "outputs", "mint", "referenceInputs", "signatories", "validRange"]
        ctx.txInfo
  cs <- tcont . plet $ P.do
    PMinting (pfield0 -> ownCS) <- pmatch ctx.purpose
    pfromData ownCS
  pauctionValHash <- tcont . plet $ pconstant auctionValHash
  refInsAsOuts <-
    tcont . plet $
      pmap # pfield @"resolved" # pfromData txInfo.referenceInputs
  outstoValidator <-
    tcont . plet $
      pfilter # (patScriptAddress # pauctionValHash) # txInfo.outputs
  -- market UTxO reference input always consumed for terms
  marketTerms <-
    tcont . plet . pfromData $
      ptryFirstJust
        # (marketInputUtxoDatum # pconstant marketCS)
        # refInsAsOuts
  market <- tcont $ pletFields @'["minAda", "percentageFee"] marketTerms

  minAdaValue <-
    tcont . plet $
      plovelace #$ padaTolovelace #$ pfromData market.minAda
  mint <- tcont . plet $ pnormalize # txInfo.mint
  signatures <- tcont . plet $ pfromData txInfo.signatories
  let common =
        MkCommon
          { cs
          , nodeCS
          , mint
          , inputs = txInfo.inputs
          , toValidator = outstoValidator
          , signatures
          , minAdaValue = minAdaValue
          }

  pure
    ( common
    , txInfo.validRange
    , marketTerms
    , refInsAsOuts
    )

---------------------------------
-- State Minting Policy common redeemers
---------------------------------

-- The only logic here is to defer validation to AuctionValidator
plifeCycle ::
  forall (s :: S).
  Vulcan.Config ->
  Term s (PValidatorHash :--> PScriptContext :--> PUnit)
plifeCycle cfg = plam $ \valHash ctx -> P.do
  let inputs :: Term _ (PBuiltinList PTxInInfo)
      inputs = pfield @"inputs" #$ pfield @"txInfo" # ctx
      inputsFromValidator = pfilter # (pfromScriptAddress # valHash) # inputs
  -- Inputs:
  passert cfg "There must be an input from AuctionValidator" $
    pnot #$ pnull # inputsFromValidator
  pconstant ()

pAnnounceAuction ::
  forall (s :: S).
  Vulcan.Config ->
  PStateCommon s ->
  Term s (PTxOutRef :--> PUnit)
pAnnounceAuction cfg common = plam $ \ref -> P.do
  -- Output:
  auctionEscrowOut <-
    plet $
      parseAuctionFromOutputs cfg
        # common.minAdaValue
        # common.cs
        # common.toValidator

  datum <- pletFields @'["terms", "nodeCS"] auctionEscrowOut
  terms <- pletFields @'["lot", "auctionInfo", "bidInfo"] datum.terms
  info <- pletFields @'["seller", "beneficiaries"] terms.auctionInfo

  let beneficiaries = pfromData $ info.beneficiaries
      beneficiaryShare = pmap # plam (\n -> pto $ psndBuiltin # n) #$ pto beneficiaries
      lot = pfromData terms.lot
      -- Minting
      -- to AuctionEscrow UTxO
      auctionToken = V.psingleton # common.cs # pnameToken @PAuctionEscrow # 1
      corrNodeToken = V.psingleton # common.nodeCS # pcorrNodeTN # 1
      -- to origin SetNode UTxO
      originNodeToken = V.psingleton # common.nodeCS # poriginNodeTN # 1

  -----------------------------
  -- Checks:

  passert cfg "Map of beneficiaries must equal 100" $
    (psumData # beneficiaryShare) #== 100

  let maxAssets = T.pack $ show @Integer Constants.maxAmountOfAssetsInLot
  passert cfg ("Amount of tokens in Lot must be less or equal to " <> maxAssets) $
    pamountOfAssets # terms.lot #<= Constants.maxAmountOfAssetsInLot

  passert cfg "No TN in lot can be AuctionEscrow" $
    pgetCS # terms.lot # pnameToken @PAuctionEscrow #== pcon PNothing

  passert cfg "Tx must be signed by seller" $
    ptxSignedBy # common.signatures #$ tryPkFromAddress # info.seller

  passert cfg "Incorrect lot with TxOutRef" $
    pany # (hasValueAndRef # ref # lot) # common.inputs

  passert cfg "Node currency symbol is incorrect" $
    pfromData datum.nodeCS #== common.nodeCS

  passert cfg "Raise Percentage is < 0" $
    0 #<= pfromData (pfield @"raisePercentage" # terms.bidInfo)

  passert cfg "Incorrect mint amounts" $
    common.mint #== auctionToken <> corrNodeToken <> originNodeToken

  pconstant ()

--------------------------------
-- Helpers:

-- Common information shared between all redeemers.
data PStateCommon (s :: S) = MkCommon
  { cs :: Term s PCurrencySymbol
  -- ^ state token (own) CS
  , nodeCS :: Term s PCurrencySymbol
  -- ^ FinSet MP CS
  , mint :: Term s (PValue 'Sorted 'NonZero)
  -- ^ value minted in current Tx
  , inputs :: Term s (PBuiltinList PTxInInfo)
  -- ^ current Tx inputs
  , toValidator :: Term s (PBuiltinList PTxOut)
  -- ^ current Tx outputs to AuctionValidator
  , signatures :: Term s (PBuiltinList (PAsData PPubKeyHash))
  -- ^ current Tx signatories
  , minAdaValue :: Term s (PValue 'Sorted 'Positive)
  -- ^ current min ADA accordingly to market terms
  }
  deriving stock (Generic)
