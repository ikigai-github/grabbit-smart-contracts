module Vulcan.Onchain.Auction.Validator.Common (
  pCancelAct,
  pBidAct,
  pBuyNowAct,
  pExtendAct,
  pCloseExtendAct,
  pRefundAct,
  pResolveAuctionAct,
  makeCommon,
) where

import Plutarch.Api.V1 (
  PCurrencySymbol,
  PScriptPurpose (PSpending),
 )
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V1.Value qualified as V
import Plutarch.Api.V2 (PMaybeData (PDJust, PDNothing), PPOSIXTime (PPOSIXTime), PScriptContext)
import Plutarch.Extra.Interval (pafter, pbefore)
import Plutarch.Monadic qualified as P

import PlutusLedgerApi.V1 (CurrencySymbol)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Onchain.Auction.Validator.Helpers (
  PValidatorCommon (..),
  auctionUnchanged,
  checkBidRaise,
  correctPaid,
  correctResolvedPayments,
  higherBidThan,
 )
import Vulcan.SpecialUTxO.Types (
  NamedToken (pnameToken),
  SpecialUTxOTag (BidUTxO),
  boughtTN,
  pcorrNodeTN,
  pnodeKeyToken,
  poriginNodeTN,
 )
import Vulcan.SpecialUTxO.Utils (
  auctionDatumFromInputs,
  auctionInputUtxoDatum,
  boughtAuctionDatumFromInputs,
  checkBoughtUtxoValue,
  marketInputUtxoDatum,
  parseAuctionFromOutputs,
  parseBidInput,
  parseBidOutput,
  pisAuction,
  pisValidBidUtxo,
  specialInputUtxo,
 )
import Vulcan.Types.Auction (
  PAuctionEscrow,
  PBidEscrow,
  PBidStatus (PBid, PNoBid),
  PCancelReason (..),
  PRefundReason (..),
 )
import Vulcan.Types.Market (PMarketTerms)
import Vulcan.Utils (
  padaTolovelace,
  passert,
  passumeWellFormedInlineDatum,
  patScriptAddress,
  pconvertWithRest,
  pfield0,
  pfindWithRest,
  plovelace,
  ptryFind,
  ptryFirstJust,
  ptryOwnHash,
  ptxSignedBy,
  traceError,
  tryPkFromAddress,
 )

makeCommon ::
  forall {r :: PType} {s :: S}.
  CurrencySymbol ->
  Term s PScriptContext ->
  TermCont @r
    s
    ( PValidatorCommon s
    , Term s PMarketTerms
    )
makeCommon marketCS ctx' = do
  ---------------------------
  -- Collecting information needed for validation:

  ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'

  txInfo <-
    tcont $
      pletFields @'["inputs", "outputs", "mint", "signatories", "validRange", "referenceInputs"]
        ctx.txInfo
  inputs <- tcont . plet $ pfromData txInfo.inputs
  outputs <- tcont . plet $ pfromData txInfo.outputs
  asOuts <- tcont . plet $ pmap # plam (pfield @"resolved" #)
  refInsAsOuts <- tcont . plet $ asOuts #$ pfromData txInfo.referenceInputs

  ownHash <- tcont . plet $ P.do
    PSpending refRecord <- pmatch ctx.purpose
    ref <- plet . pfromData . pfield0 $ refRecord
    ptryOwnHash # inputs # ref

  atOwnAddress <- tcont . plet $ pfilter #$ patScriptAddress # ownHash

  refAsOutFromValidator <- tcont . plet $ atOwnAddress # refInsAsOuts
  txOutsFromValidator <- tcont . plet $ atOwnAddress #$ asOuts # inputs
  txOutstoValidator <- tcont . plet $ atOwnAddress # outputs

  -- market is always referenced for fees and min ADA information
  market <-
    tcont . plet . pfromData $
      ptryFirstJust
        # (marketInputUtxoDatum # pconstant marketCS)
        # refInsAsOuts
  sharedMarketFields <-
    tcont $ pletFields @'["minAda", "percentageFee"] market

  minAdaValue <-
    tcont . plet $
      plovelace
        #$ padaTolovelace
        #$ pfromData sharedMarketFields.minAda
  mint <- tcont . plet $ pnormalize # txInfo.mint
  let common =
        MkCommon
          { sigs = txInfo.signatories
          , mint
          , range = txInfo.validRange
          , ins = inputs
          , refAtVal = refAsOutFromValidator
          , outs = outputs
          , fromValidator = txOutsFromValidator
          , toValidator = txOutstoValidator
          , minAda = minAdaValue
          }
  pure (common, market)

---------------------------
-- Common Redeemers:
---------------------------

pCancelAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (PCancelReason :--> PCurrencySymbol :--> PUnit)
pCancelAct cfg common = plam $ \reason cs -> P.do
  -----------------------------
  -- Inputs:
  let auctionIn = pmatch reason $ \case
        PFailed _ -> auctionDatumFromInputs cfg # cs # common.fromValidator
        PBought _ -> boughtAuctionDatumFromInputs cfg # cs # common.fromValidator

  auctionEscrow <- pletFields @["terms", "nodeCS"] $ auctionIn
  terms <- pletFields @'["auctionInfo", "lot"] $ auctionEscrow.terms
  let nodeCS = pfromData auctionEscrow.nodeCS
      seller = pfield @"seller" # terms.auctionInfo
      doubleMinAda = common.minAda <> common.minAda
      -----------------------------
      -- Minting:
      -- from AuctionEscrow UTxO:
      auctionToken = V.psingleton # cs # pnameToken @PAuctionEscrow # (-1)
      corrNodeToken = V.psingleton # nodeCS # pcorrNodeTN # (-1)
      -- from its SetNode:
      originNodeToken = V.psingleton # nodeCS # poriginNodeTN # (-1)
      baseMint = auctionToken <> originNodeToken <> corrNodeToken
  PPair mintAmount refundAmount <- pmatch $
    pmatch reason $ \case
      PFailed _ ->
        let lot = pfromData terms.lot
         in pcon $ PPair baseMint (lot <> doubleMinAda)
      PBought _ ->
        let boughtToken = V.psingleton # cs # boughtTN # (-1)
         in pcon $ PPair (baseMint <> boughtToken) doubleMinAda

  -----------------------------
  -- Checks:

  passert cfg "Only AuctionEscrow UTxO must be spent from validator" $
    plength # common.fromValidator #== 1

  passert cfg "Incorrect mint amounts" $
    common.mint #== mintAmount

  passert cfg "Incorrect refund" $
    let isRefunded = correctPaid # seller #$ refundAmount
     in pany # isRefunded # common.outs

  pmatch reason $ \case
    PFailed _ -> P.do
      passert cfg "Tx must be signed by seller" $
        ptxSignedBy # common.sigs #$ tryPkFromAddress # seller
      pconstant ()
    PBought _ -> pconstant ()

-------------------------------------------

pBidAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (PCurrencySymbol :--> PMarketTerms :--> PUnit)
pBidAct cfg common = plam $ \cs marketTerms -> P.do
  -----------------------------
  -- Reference Inputs:
  terms <-
    pletFields @'["bidInfo", "time", "auctionInfo"] $
      pfield @"terms" #$ auctionDatumFromInputs cfg # cs # common.refAtVal

  time <- pletFields @'["close", "start"] terms.time
  let sellerToCover = pfromData $ pfield @"sellerToCover" # terms.auctionInfo
      startingPrice = pfromData $ pfield @"startingPrice" # terms.bidInfo
      start = pfromData time.start
      close = pfromData time.close

  -----------------------------
  -- Inputs:

  -- Continuing input
  bidInDatum <-
    pletFields @'["status", "bidder"] $
      pfield @"datum"
        #$ parseBidInput # common.minAda # cs # common.fromValidator

  --------------------------------
  -- Outputs:
  bidOut <-
    plet $
      parseBidOutput # common.minAda # cs # common.toValidator

  bidOutDatum <-
    pletFields @'["bidder", "status"] $
      pfield @"datum" # bidOut
  bidAmount <- plet $ P.do
    PBid bidState <- pmatch $ pfromData bidOutDatum.status
    pfromData $ pfield @"bid" # bidState

  --------------------------------
  -- Checks:

  passert cfg "Must spend only BidEscrow UTxO from AuctionValidator" $
    plength # common.fromValidator #== 1

  passert cfg "Tx must be signed by bidder" $
    ptxSignedBy # common.sigs #$ tryPkFromAddress # bidInDatum.bidder

  passert cfg "Tx must not mint" $ common.mint #== mempty

  passert cfg "Bid UTxO must be valid" $
    pisValidBidUtxo
      # bidOut
      # sellerToCover
      # startingPrice
      # marketTerms
      # start
      # close
      # common.range

  passert cfg "Invalid bid raise" $
    checkBidRaise cfg # terms.bidInfo # bidInDatum.status # bidAmount

  passert cfg "Bidder must not change" $
    bidOutDatum.bidder #== bidInDatum.bidder

  pconstant ()

-------------------------------

pBuyNowAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (PCurrencySymbol :--> PMarketTerms :--> PUnit)
pBuyNowAct cfg common = plam $ \cs market -> P.do
  --------------------------------
  -- Inputs:
  PPair auctionIn rest <-
    pmatch $ pfindWithRest # (pisAuction # cs) # common.fromValidator

  PPair pbidIn extraInputsFromValidator <-
    pmatch $
      pconvertWithRest
        # (specialInputUtxo BidUTxO # common.minAda # cs)
        # rest

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
  -- Extracting data from bid:
  bidDatum <- pletFields @'["bidder", "status"] $ pfield @"datum" # pbidIn
  let bidder = tryPkFromAddress # bidDatum.bidder

  --------------------------------
  -- Outputs:
  auctionOut <- plet $ ptryFind # (pisAuction # cs) # common.toValidator

  let --------------------------------
      -- Minting:
      -- from destroyed BidEscrow UTxO:
      bidToken = V.psingleton # cs # pnameToken @PBidEscrow # (-1)
      nodeToken = pnodeKeyToken # nodeCS # bidder # (-1)
      -- to AuctionEscrow UTxO:
      boughtToken = V.psingleton # cs # boughtTN # 1

  -------------------------------
  -- Checks:

  passert cfg "Only AuctionEscrow and BidEscrow should be spent from AuctionValidator" $
    pnull # extraInputsFromValidator

  passert cfg "Tx must be signed by bidder" $
    ptxSignedBy # common.sigs # bidder

  passert cfg "Incorrect mint amounts" $
    common.mint #== bidToken <> nodeToken <> boughtToken

  passert cfg "Incorrect bought auction value" $
    checkBoughtUtxoValue # common.minAda # cs # nodeCS # auctionOut

  passert cfg "Datum must not be altered" $
    auctionInDatum #== (pfield @"datum" # auctionOut)

  correctResolvedPayments cfg
    # buyNowPrice
    # pbidIn
    # market
    # auctionEscrow.terms
    # common.outs

-----------------------------------

pExtendAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (PCurrencySymbol :--> PUnit)
pExtendAct cfg common = plam $ \cs -> P.do
  -- Reference Inputs:
  -- time of bid witnesses bidders activity around auction close

  PBid status <-
    pmatch . pfromData $
      pfield @"status"
        #$ pfield @"datum"
        #$ parseBidInput # common.minAda # cs # common.refAtVal
  bidStatus <- pletFields @'["time", "bid"] status
  let bidTime = pfromData bidStatus.time

  -- Inputs
  auctionIn <- plet $ auctionDatumFromInputs cfg # cs # common.fromValidator

  oldTerms <- pletFields @'["time", "bidInfo", "auctionInfo"] $ pfield @"terms" # auctionIn
  oldBidInfo <-
    pletFields @'["buyNowPrice", "raiseMinimum", "raisePercentage"] oldTerms.bidInfo
  oldTime <- pletFields @'["extension", "close", "start"] oldTerms.time
  -- check that the auction is extendable
  PDJust (pfield0 -> extension) <- pmatch $ pfromData oldTime.extension
  auctionExtension <- pletFields @'["window", "length"] extension
  close <- plet $ pfromData oldTime.close
  let window = pto $ pfromData auctionExtension.window
      extension = pto $ pfromData auctionExtension.length
      updatedClose = pcon $ PPOSIXTime $ pto close + extension
      bid = pfromData bidStatus.bid
      bidRaise = pfromData oldBidInfo.raiseMinimum
      updatedStartingPrice = bid + bidRaise

  -- Outputs:
  auctionOut <- plet $ parseAuctionFromOutputs cfg # common.minAda # cs # common.toValidator
  newTerms <- pletFields @'["time", "bidInfo", "auctionInfo"] $ pfield @"terms" # auctionOut
  newBidInfo <-
    pletFields @'["buyNowPrice", "startingPrice", "raiseMinimum", "raisePercentage"] newTerms.bidInfo
  newTime <- pletFields @'["extension", "close", "start"] newTerms.time

  ---------------------------------
  -- Checks:

  passert cfg "Only AuctionEscrow must be spent from AuctionValidator" $
    plength # common.fromValidator #== 1

  passert cfg "Auction has finished" $ pafter # close # common.range

  passert cfg "Tx must not mint" $ common.mint #== mempty

  passert cfg "Bid must be in auction extension window" $
    pto close - window #< pto bidTime

  passert cfg "Starting Price incorrectly updated" $
    pfromData newBidInfo.startingPrice #== updatedStartingPrice

  passert cfg "Close time incorrectly updated" $
    pfromData newTime.close #== updatedClose

  passert cfg "AuctionTerms modified" $
    auctionUnchanged # auctionIn # auctionOut
      #&& newTime.start #== oldTime.start
      #&& newTime.extension #== oldTime.extension
      #&& newBidInfo.buyNowPrice #== oldBidInfo.buyNowPrice
      #&& newBidInfo.raiseMinimum #== oldBidInfo.raiseMinimum
      #&& newBidInfo.raisePercentage #== oldBidInfo.raisePercentage

  pconstant ()

--------------------------------------

pCloseExtendAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (PCurrencySymbol :--> PUnit)
pCloseExtendAct cfg common = plam $ \cs -> P.do
  -- Inputs
  auctionIn <- plet $ auctionDatumFromInputs cfg # cs # common.fromValidator

  oldTerms <- pletFields @'["time", "bidInfo", "auctionInfo"] $ pfield @"terms" # auctionIn
  let seller = pfield @"seller" # oldTerms.auctionInfo
  oldTime <- pletFields @'["extension", "close", "start"] $ oldTerms.time
  -- check that the auction is extendable
  PDJust _ <- pmatch $ pfromData oldTime.extension

  -- Outputs:
  auctionOut <- plet $ parseAuctionFromOutputs cfg # common.minAda # cs # common.toValidator
  newTerms <- pletFields @'["time", "bidInfo", "auctionInfo"] $ pfield @"terms" # auctionOut
  newTime <- pletFields @'["extension", "close", "start"] newTerms.time

  -- Check that the auction Extension possibility has been removed
  PDNothing _ <- pmatch $ pfromData newTime.extension

  ---------------------------------
  -- Checks:

  passert cfg "Only AuctionEscrow must be spent from AuctionValidator" $
    plength # common.fromValidator #== 1

  passert cfg "Tx must not mint" $ common.mint #== mempty

  passert cfg "Tx must be signed by seller" $
    ptxSignedBy # common.sigs #$ tryPkFromAddress # seller

  passert cfg "AuctionEscrow modified" $
    auctionUnchanged # auctionIn # auctionOut
      #&& newTime.close #== oldTime.close
      #&& newTime.start #== oldTime.start
      #&& newTerms.bidInfo #== oldTerms.bidInfo

  pconstant ()

--------------------------------------

pRefundAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (PRefundReason :--> PCurrencySymbol :--> PUnit)
pRefundAct cfg common = plam $ \reason cs -> P.do
  -- Reference Inputs:
  auctionEscrow <- pletFields @'["terms", "nodeCS"] $ P.do
    pmatch reason $ \case
      PBoughtLot _ -> boughtAuctionDatumFromInputs cfg # cs # common.refAtVal
      PLostBid _ -> auctionDatumFromInputs cfg # cs # common.refAtVal

  let nodeCS = pfromData auctionEscrow.nodeCS

  -- Inputs:
  bidUtxo <-
    pletFields @'["datum", "extraValue"] $
      -- must be destroyed
      parseBidInput # common.minAda # cs # common.fromValidator

  bidDatum <- pletFields @'["bidder", "status"] bidUtxo.datum
  bidder <- plet $ tryPkFromAddress # bidDatum.bidder

  -- Outputs checked in Checks section

  let refund = pfromData bidUtxo.extraValue <> common.minAda
      isRefunded = correctPaid # bidDatum.bidder # refund
      -- Minting:
      -- from destroyed BidEscrow UTxO:
      bidToken = V.psingleton # cs # pnameToken @PBidEscrow # (-1)
      -- from it's destroyed SetNode UTxO:
      nodeToken = pnodeKeyToken # nodeCS # bidder # (-1)

  ----------------------------
  -- Checks:

  passert cfg "Only BidEscrow UTxO may be spent from AuctionValidator" $
    plength # common.fromValidator #== 1

  passert cfg "Incorrect mint amounts" $ common.mint #== bidToken <> nodeToken

  -- checking outputs
  passert cfg "Loser not refunded" $ pany # isRefunded # common.outs

  pmatch reason $ \case
    PBoughtLot _ ->
      -- inputs and outputs for this case are already checked
      pconstant ()
    PLostBid _ -> P.do
      let close =
            pfromData $
              pfield @"close"
                #$ pfield @"time" # auctionEscrow.terms
          auctionClosed = pbefore # close # common.range
          signed = ptxSignedBy # common.sigs # bidder
          hasNoBid = pmatch (pfromData bidDatum.status) $ \case
            PBid _ -> pcon PFalse
            PNoBid _ -> pcon PTrue

      passert
        cfg
        "Lost bid may only be refunded if auction closed or \
        \if Tx signed by the bidder"
        $ auctionClosed #|| signed

      pif hasNoBid (pconstant ()) $ P.do
        -- to ensure that higher bid exists
        higherBidDatum <-
          plet $
            pfield @"datum"
              #$ parseBidInput # common.minAda # cs # common.refAtVal
        passert cfg "The provided bid must be higher" $
          higherBidThan # higherBidDatum # bidUtxo.datum

        passert cfg "The reference bid must not be the bid" $
          pnot #$ higherBidDatum #== bidUtxo.datum

        pconstant ()

-----------------------------

pResolveAuctionAct ::
  forall (s :: S).
  Vulcan.Config ->
  PValidatorCommon s ->
  Term s (PCurrencySymbol :--> PMarketTerms :--> PUnit)
pResolveAuctionAct cfg common = plam $ \cs market -> P.do
  -- Inputs:
  -- AuctionEscrow UTxO to destroy
  PPair auctionIn rest <-
    pmatch $
      pconvertWithRest
        # (auctionInputUtxoDatum cfg # cs)
        # common.fromValidator
  auctionEscrow <- pletFields @["terms", "nodeCS"] auctionIn
  let time = pfield @"time" # auctionEscrow.terms
      nodeCS = pfromData auctionEscrow.nodeCS
      close = pfromData $ pfield @"close" # time

  -- winner BidEscrow UTxO to destroy
  PPair bidIn extraInputsFromValidator <-
    pmatch $
      pconvertWithRest
        # (specialInputUtxo BidUTxO # common.minAda # cs)
        # rest
  bidDatum <-
    pletFields @'["bidder", "status"] $
      pfield @"datum" # bidIn
  let bidder = tryPkFromAddress # bidDatum.bidder

  ----------------------------
  -- Mint:
  -- all those tokens come from inputs UTxOs that will be destroyed
  -- tokens must be burned
  let -- from AuctionEscrow UTxO:
      auctionToken = V.psingleton # cs # pnameToken @PAuctionEscrow # (-1)
      corrNodeToken = V.psingleton # nodeCS # pcorrNodeTN # (-1)
      -- from SetNode UTxO corresponding to AuctionEscrow:
      originToken = V.psingleton # nodeCS # poriginNodeTN # (-1)
      -- from winner BidEscrow UTxO:
      bidToken = V.psingleton # cs # pnameToken @PBidEscrow # (-1)
      -- from SetNode UTxO corresponding to winner BidEscrow UTxO
      nodeToken = pnodeKeyToken # nodeCS # bidder # (-1)
      toMint =
        auctionToken <> bidToken <> nodeToken <> originToken <> corrNodeToken

  pmatch (pfromData bidDatum.status) $ \case
    PNoBid _ ->
      {- Auctions can only be resolved with a valid bid. If NoBid
      then the bid must be refunded and the auction cancelled
      -}
      traceError cfg "No Bid has been made"
    PBid n -> P.do
      bidAmount <- plet $ padaTolovelace #$ pfromData $ pfield @"bid" # n

      -------------------------------
      -- Checks:

      passert cfg "Only AuctionEscrow and BidEscrow must be spent from AuctionValidator" $
        pnull # extraInputsFromValidator

      passert cfg "Incorrect mint" $ common.mint #== toMint

      passert cfg "Auction has not finished" $ pbefore # close # common.range

      correctResolvedPayments cfg
        # bidAmount
        # bidIn
        # market
        # auctionEscrow.terms
        # common.outs

---------------------------------------------------
