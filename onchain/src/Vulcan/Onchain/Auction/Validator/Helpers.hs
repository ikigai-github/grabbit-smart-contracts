module Vulcan.Onchain.Auction.Validator.Helpers (
  PValidatorCommon (..),
  correctPaid,
  checkBidRaise,
  higherBidThan,
  correctResolvedPayments,
  paymentToBeneficiaries,
  auctionUnchanged,
  calcFees,
  correctResolvedPaymentsNoEnroll,
) where

import Plutarch.Api.V1 (
  AmountGuarantees (NonZero, Positive),
  PInterval,
  PMap,
  PPOSIXTime,
  PValue,
 )
import Plutarch.Api.V1.Value (passertPositive, plovelaceValueOf)
import Plutarch.Api.V2 (
  KeyGuarantees (Sorted, Unsorted),
  PAddress,
  PPubKeyHash,
  PTxInInfo,
  PTxOut,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Positive (PPositive, ppositive, ptryPositive)
import Plutarch.Rational (ptruncate)

import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.SpecialUTxO.Types (PUTxO)
import Vulcan.Types.Auction (
  PAuctionEscrow,
  PAuctionTerms,
  PBidEscrow,
  PBidInfo,
  PBidStatus (PBid, PNoBid),
 )
import Vulcan.Types.Market (PMarketTerms)
import Vulcan.Utils (
  padaTolovelace,
  passert,
  percentage,
  pfindWithRest,
  plovelace,
  ptoAddress,
  (#-),
 )

---------------------------
-- Helpers:
---------------------------

---------------------------
-- parsing

{- | Checks to see if a bid raise is valid by checking it against the BidInfo
     held within the auction Terms
-}
checkBidRaise ::
  Vulcan.Config ->
  ClosedTerm
    ( PBidInfo
        :--> PBidStatus
        :--> PPositive
        :--> PBool
    )
checkBidRaise cfg = phoistAcyclic $
  plam $ \bidInc' status newBid -> P.do
    bidInc <- pletFields @'["raiseMinimum", "raisePercentage", "buyNowPrice"] bidInc'
    passert cfg "Bid cannot be more than BuyNow Price" $
      newBid #< pfromData bidInc.buyNowPrice
    pmatch status $ \case
      PNoBid _ -> pcon PTrue
      PBid n ->
        let currentBid = pfromData $ pfield @"bid" # n
            byPercentage = raisePercentage # (newBid - currentBid) # currentBid
         in (pfromData bidInc.raisePercentage) #<= pto byPercentage
              #&& pfromData bidInc.raiseMinimum #<= (newBid - currentBid)
              #&& currentBid #< newBid
        where
          raisePercentage :: Term s (PPositive :--> PPositive :--> PPositive)
          raisePercentage = plam $ \raise total ->
            let rational = pcon $ PRational (pto raise * 100) total
             in ptryPositive #$ ptruncate # rational

{- | Checks that the first bid is higher than the second.
  The first bid is higher if both bids are empty,
  or equal and have been made at the same time.
-}
higherBidThan :: ClosedTerm (PBidEscrow :--> PBidEscrow :--> PBool)
higherBidThan = phoistAcyclic $
  plam $ \higherBid lowerBid -> P.do
    higherBidStatus <- plet $ pfield @"status" # higherBid
    lowerBidStatus <- plet $ pfield @"status" # lowerBid
    pmatch lowerBidStatus $ \case
      PNoBid _ -> pcon PTrue
      PBid lose -> pmatch higherBidStatus $ \case
        PNoBid _ -> pcon PFalse
        PBid win -> P.do
          losing <- pletFields @'["bid", "time"] lose
          winning <- pletFields @'["bid", "time"] win
          losingBid <- plet $ pfromData losing.bid
          winningBid <- plet $ pfromData winning.bid
          let madeBidEarlier =
                -- smaller timestampt - earlier bid
                pfromData winning.time #<= pfromData losing.time
              wonByBid = losingBid #< winningBid
              deadHeat = losingBid #== winningBid
           in wonByBid #|| (deadHeat #&& madeBidEarlier)

-- | Ensure that atleast the correct minimum Value is paid to a certain Address
correctPaid ::
  ClosedTerm
    (PAsData PAddress :--> PValue 'Sorted 'Positive :--> PTxOut :--> PBool)
correctPaid = plam $ \add val out ->
  (ptoAddress # add # out)
    #&& val #<= pfromData (pfield @"value" # out)

{- | Checks that the correct payments are paid out to all parties when the lot has
   been succesfully bought

   @correctResolvedPayments amount bid market terms txOuts@
   - `amount` Either the winning bid or buyNow amount
   - `bid` The winning bidEscrow
   - `market` The Market Terms used to calculate fees
   - `terms` The Auction Terms
   - `txOuts` The list of TxOuts as outputs
-}
correctResolvedPayments ::
  Vulcan.Config ->
  ClosedTerm
    ( PPositive
        :--> PUTxO PBidEscrow
        :--> PMarketTerms
        :--> PAuctionTerms
        :--> PBuiltinList PTxOut
        :--> PUnit
    )
correctResolvedPayments cfg = phoistAcyclic $
  plam $ \bidAmount bid' market' terms' outs -> P.do
    market <- pletFields @'["minAda", "feeAddress"] market'
    terms <- pletFields @'["lot", "auctionInfo"] $ terms'
    auctionInfo <-
      pletFields @'["sellerToCover", "beneficiaries"] terms.auctionInfo
    bid <- pletFields @'["datum", "extraValue"] bid'
    bidValue <- plet $ plovelace # bidAmount

    minAdaValue <-
      plet $
        plovelace #$ padaTolovelace #$ pfromData market.minAda
    bensPayFees <- plet $ pfromData auctionInfo.sellerToCover
    fees <- plet $ calcFees # market' # pto bidAmount

    let bidder = pfield @"bidder" # bid.datum
        beneficiaries = pfromData auctionInfo.beneficiaries
        minLotUtxoValue = pfromData terms.lot <> minAdaValue
        minusFees ada = passertPositive #$ ada #- fees
        toWinner =
          -- If buyNow then the bid.extraValue may be less than the bid/buyNow
          pif (bid.extraValue #< bidValue) minLotUtxoValue $
            let restOfBidEscrow = passertPositive #$ bid.extraValue #- bidValue
                subtractFees from = pif bensPayFees from (minusFees from)
             in subtractFees $ restOfBidEscrow <> minLotUtxoValue
        toBens =
          let subtractFees from = pif bensPayFees (minusFees from) from
           in subtractFees $ bidValue <> minAdaValue <> minAdaValue

        -- predicates to check if TxOut is correct payment to _
        correctToWinner = correctPaid # bidder # toWinner
        correctToMarket = correctPaid # market.feeAddress # fees

    -- Check for and then remove the market UTxO provided fees are required
    withoutMarket <- plet $
      pif (fees #== mempty) outs $ P.do
        PPair _ noMarket <-
          pmatch $ pfindWithRest # correctToMarket # outs
        noMarket

    -- Check for and then remove the winners UTxO
    PPair _ withoutMarketAndWinner <-
      pmatch $ pfindWithRest # correctToWinner # withoutMarket

    passert cfg "Payments to beneficiaries is incorrect" $
      paymentToBeneficiaries # beneficiaries # toBens # withoutMarketAndWinner

    pconstant ()

{- | Checks that the correct payments are paid out to all parties when the lot has
   been succesfully bought by a party that was not yet enrolled in the auction

   @correctResolvedPaymentsNoEnroll amount bid market terms txOuts@
   - `amount` The buyNow amount
   - `receiver` The address designated to receive the auctioned lot.
   - `market` The Market Terms used to calculate fees
   - `terms` The Auction Terms
   - `txOuts` The list of TxOuts as outputs
-}
correctResolvedPaymentsNoEnroll ::
  Vulcan.Config ->
  ClosedTerm
    ( PPositive
        :--> PAsData PAddress
        :--> PMarketTerms
        :--> PAuctionTerms
        :--> PBuiltinList PTxOut
        :--> PUnit
    )
correctResolvedPaymentsNoEnroll cfg = phoistAcyclic $
  plam $ \bidAmount receiver market' terms' outs -> P.do
    market <- pletFields @'["minAda", "feeAddress"] market'
    terms <- pletFields @'["lot", "auctionInfo"] $ terms'
    auctionInfo <-
      pletFields @'["sellerToCover", "beneficiaries"] terms.auctionInfo
    bidValue <- plet $ plovelace # bidAmount

    minAdaValue <-
      plet $
        plovelace #$ padaTolovelace #$ pfromData market.minAda
    bensPayFees <- plet $ pfromData auctionInfo.sellerToCover
    fees <- plet $ calcFees # market' # pto bidAmount

    let beneficiaries = pfromData auctionInfo.beneficiaries
        minusFees ada = passertPositive #$ ada #- fees
        toBens =
          let subtractFees from = pif bensPayFees (minusFees from) from
           in subtractFees $ bidValue <> minAdaValue <> minAdaValue
        toWinner = pfromData terms.lot

        -- predicates to check if TxOut is correct payment to _
        correctToWinner = correctPaid # receiver # toWinner
        correctToMarket = correctPaid # market.feeAddress # fees

    withoutMarket <- plet $
      pif (fees #== mempty) outs $ P.do
        PPair _ noMarket <-
          pmatch $ pfindWithRest # correctToMarket # outs
        noMarket

    -- Check for and then remove the winners UTxO
    PPair _ withoutMarketAndWinner <-
      pmatch $ pfindWithRest # correctToWinner # withoutMarket

    passert cfg "Payments to beneficiaries is incorrect" $
      paymentToBeneficiaries # beneficiaries # toBens # withoutMarketAndWinner

    pconstant ()

{- | Ensures that every beneficiary gets paid at least the minimum expected Value.

  @paymentToBeneficiaries bens total utxos@
  - `bens` The Map from beneficiary to their part of the total value to be paid, in percentage
  - `total` The total value to be paid
  - `utxos` The list of UTxOs to look for payments in
-}
paymentToBeneficiaries ::
  ClosedTerm
    ( PMap 'Unsorted PAddress PPositive
        :--> PValue 'Sorted 'Positive
        :--> PBuiltinList PTxOut
        :--> PBool
    )
paymentToBeneficiaries = phoistAcyclic $
  plam $ \bens bid txOuts -> P.do
    beneficiaries <- plet $ pto bens
    correctToBen <- plet $
      plam $ \outs ben ->
        let correctReward out =
              let benAddress = pfstBuiltin # ben
                  benPart = pto $ pfromData $ psndBuiltin # ben
                  bidValue = plovelaceValueOf # bid
                  outValue = plovelaceValueOf #$ pfield @"value" # out
                  goesToBen = ptoAddress # benAddress # out
               in goesToBen #&& (percentage # bidValue # benPart) #<= outValue
         in pany # plam correctReward # outs
    pall # (correctToBen # txOuts) # beneficiaries

{- | Calculates the fees as a value in lovelace

  @calcFees market bid@
  - `market` - market terms
  - `bid` - the size of a bid to count fees for in lovelace
-}
calcFees :: ClosedTerm (PMarketTerms :--> PInteger :--> PValue 'Sorted 'Positive)
calcFees = plam $ \market' bid -> P.do
  market <- pletFields @'["fixedFee", "percentageFee"] market'
  let fixedFeeAmount = padaTolovelace # pfromData market.fixedFee
      bidFee = percentage # pfromData market.percentageFee # bid
      fees = ppositive #$ fixedFeeAmount + bidFee
  pmatch fees $ \case
    PJust fee -> plovelace # fee
    PNothing -> mempty

-- | Checks that the AuctionEscrow remains unaltered besides time and bidInfo.
auctionUnchanged :: Term s (PAsData PAuctionEscrow :--> PAsData PAuctionEscrow :--> PBool)
auctionUnchanged = phoistAcyclic $
  plam $ \old new -> P.do
    oldEscrow <- pletFields @'["nodeCS", "terms"] old
    oldTerms <-
      pletFields
        @'["lot", "auctionInfo"]
        oldEscrow.terms

    newEscrow <- pletFields @'["nodeCS", "terms"] new
    newTerms <-
      pletFields
        @'["lot", "auctionInfo"]
        newEscrow.terms
    newEscrow.nodeCS #== oldEscrow.nodeCS
      #&& newTerms.lot #== oldTerms.lot
      #&& newTerms.auctionInfo #== oldTerms.auctionInfo

-----------------------------
-- Common runtime information shared between all redeemers.

data PValidatorCommon (s :: S) = MkCommon
  { sigs :: Term s (PBuiltinList (PAsData PPubKeyHash))
  -- ^ Tx signatories
  , mint :: Term s (PValue 'Sorted 'NonZero)
  -- ^ Tx minting
  , range :: Term s (PInterval PPOSIXTime)
  -- ^ Tx validity range
  , ins :: Term s (PBuiltinList PTxInInfo)
  -- ^ all Tx inputs
  , outs :: Term s (PBuiltinList PTxOut)
  -- ^ all Tx outputs
  , refAtVal :: Term s (PBuiltinList PTxOut)
  -- ^ all Tx reference inputs
  , fromValidator :: Term s (PBuiltinList PTxOut)
  -- ^ inputs from AuctionValidator
  , toValidator :: Term s (PBuiltinList PTxOut)
  -- ^ outputs to AuctionValidator
  , minAda :: Term s (PValue 'Sorted 'Positive)
  -- ^ current min ADA value by market terms
  }
  deriving stock (Generic)
