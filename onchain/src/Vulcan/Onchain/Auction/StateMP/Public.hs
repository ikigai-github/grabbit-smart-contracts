module Vulcan.Onchain.Auction.StateMP.Public (
  pubStateMP,
  mkPubStateMP,
  pPubStateMP,
) where

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PInterval,
  PPOSIXTime,
 )
import Plutarch.Api.V1.Value qualified as V
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  MintingPolicy,
  TxOutRef,
  ValidatorHash,
 )

import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Onchain.Auction.StateMP.Common (
  PStateCommon (cs, minAdaValue, mint, nodeCS, signatures, toValidator),
  makeCommon,
  pAnnounceAuction,
  plifeCycle,
 )
import Vulcan.SpecialUTxO.Types (
  NamedToken (pnameToken),
  pnodeKeyToken,
 )
import Vulcan.SpecialUTxO.Utils (
  auctionDatumFromInputs,
  parseBidOutput,
  pisValidBidUtxo,
 )
import Vulcan.Types.Auction (
  PBidEscrow,
 )
import Vulcan.Types.Market (PMarketTerms)
import Vulcan.Types.State (
  PPubStateMintingRedeemer (
    PPubAnnounceAuction,
    PPubAuctionLifeCycle,
    PPubEnrollBidder
  ),
 )
import Vulcan.Utils (
  passert,
  ptxSignedBy,
  pverifyDataC,
  toPlutarchConfig,
  tryPkFromAddress,
  wrapMintingPolicy,
 )

---------------------------
-- Minting Policy:
---------------------------

mkPubStateMP ::
  forall (s :: S).
  Vulcan.Config ->
  ValidatorHash ->
  CurrencySymbol ->
  Term
    s
    ( PCurrencySymbol
        :--> PTxOutRef
        :--> PPubStateMintingRedeemer
        :--> PScriptContext
        :--> PUnit
    )
mkPubStateMP cfg auctionValHash marketCS = plam $ \nodeCS ref redm ctx ->
  pmatch redm $ \case
    PPubAuctionLifeCycle _ -> plifeCycle cfg # pconstant auctionValHash # ctx
    otherRedeemers -> P.do
      ( common
        , validRange
        , market
        , refInsAsOuts
        ) <-
        runTermCont $
          makeCommon auctionValHash marketCS nodeCS ctx
      case otherRedeemers of
        PPubAnnounceAuction _ -> pAnnounceAuction cfg common # ref
        PPubEnrollBidder _ ->
          let range = pfromData validRange
           in pEnrollAct cfg common # refInsAsOuts # range # market

---------------------------
-- Public unique redeemers:
---------------------------

pEnrollAct ::
  forall (s :: S).
  Vulcan.Config ->
  PStateCommon s ->
  Term s (PBuiltinList PTxOut :--> PInterval PPOSIXTime :--> PMarketTerms :--> PUnit)
pEnrollAct cfg common = plam $ \refIns range marketTerms -> P.do
  -- Reference Inputs:
  -- AuctionEscrow
  terms <-
    pletFields @'["lot", "time", "bidInfo", "auctionInfo"] $
      pfield @"terms" #$ auctionDatumFromInputs cfg # common.cs # refIns
  time <- pletFields @'["start", "close"] terms.time
  let sellerToCover = pfromData $ pfield @"sellerToCover" # terms.auctionInfo
      startingPrice = pfromData $ pfield @"startingPrice" # terms.bidInfo
      start = pfromData $ time.start
      close = pfromData $ time.close

  -- Outputs:
  bidOut <-
    plet $ parseBidOutput # common.minAdaValue # common.cs # common.toValidator

  bidder <- plet $ pfield @"bidder" #$ pfield @"datum" # bidOut
  bidderPKH <- plet $ tryPkFromAddress # bidder

  let -------------------------
      -- Mint:
      -- sent to new BidEscrow UTxO
      bidToken = V.psingleton # common.cs # pnameToken @PBidEscrow # 1
      -- sent to it's SetNode UTxO
      nodeToken = pnodeKeyToken # common.nodeCS # bidderPKH # 1

  -----------------------------
  -- Checks:

  passert cfg "Tx must be signed by the bidder" $
    ptxSignedBy # common.signatures # bidderPKH

  passert cfg "Bid UTxO must be valid" $
    pisValidBidUtxo
      # bidOut
      # sellerToCover
      # startingPrice
      # marketTerms
      # start
      # close
      # range

  passert cfg "Incorrect mint amounts" $
    common.mint #== bidToken <> nodeToken

  pconstant ()

------------------------------------------
-- Scripts

pPubStateMP ::
  Vulcan.Config ->
  ValidatorHash ->
  CurrencySymbol ->
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PTxOutRef
        :--> PMintingPolicy
    )
pPubStateMP cfg auctionVH pmarketCS =
  plam $ \nodeCS oref redm' ctx -> unTermCont $ do
    redm <- pverifyDataC @PPubStateMintingRedeemer redm'
    pure . popaque $
      mkPubStateMP cfg auctionVH pmarketCS
        # pfromData nodeCS
        # oref
        # pfromData redm
        # ctx

-- | Compile the State Token Minting Policy
pubStateMP ::
  Vulcan.Config ->
  ValidatorHash ->
  CurrencySymbol ->
  CurrencySymbol ->
  TxOutRef ->
  MintingPolicy
pubStateMP config auctionVH pmarketCS nodeCS oref =
  wrapMintingPolicy (toPlutarchConfig config) $
    mkPubStateMP config auctionVH pmarketCS
      # pconstant nodeCS
      # pconstant oref
