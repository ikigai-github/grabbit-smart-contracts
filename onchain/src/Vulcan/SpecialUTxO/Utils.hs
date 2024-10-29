{-# LANGUAGE AllowAmbiguousTypes #-}

module Vulcan.SpecialUTxO.Utils (
  pisAuction,
  auctionDatumFromInputs,
  parseBidInput,
  pisValidBidUtxo,
  parseSpecOutputUtxo,
  specialInputUtxo,
  parseAuctionOutputUtxo,
  boughtAuctionInputUtxoDatum,
  boughtAuctionDatumFromInputs,
  parseRegistrationOutput,
  marketInputUtxoDatum,
  parseMarketOutputUtxo,
  parseNodeOutputUtxo,
  checkBoughtUtxoValue,
  parseAuctionCS,
  nodeInputUtxoDatum,
  auctionInputUtxoDatum,
  parseAuctionFromOutputs,
  registrationInputUtxoDatum,
  parseBidOutput,
  ptokenValue,
) where

import Plutarch.Api.V1 qualified as V1
import Plutarch.Api.V1.Value as Value (PValue, passertPositive, plovelaceValueOf, pnoAdaValue)
import Plutarch.Api.V2 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  POutputDatum (POutputDatum),
  PTxOut,
 )
import Plutarch.Monadic qualified as P

import Plutarch.Extra.Interval (pafter, pbefore)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.SpecialUTxO.Types (
  NamedToken (pnameToken),
  PUTxO,
  SpecialUTxOTag (BidUTxO, RegUTxO),
  boughtTN,
  mkUTxO,
  pcorrNodeTN,
  pparseNodeKey,
 )
import Vulcan.Types.Auction (
  PAuctionEscrow,
  PBidEscrow,
  PBidStatus (PBid, PNoBid),
  PPositive,
  PRegistrationEscrow,
 )
import Vulcan.Types.FinSet (
  PNodeKey (PEmpty, PKey),
  PSetNode,
  validNode,
 )
import Vulcan.Types.Market (PMarketTerms)
import Vulcan.Utils (
  padaTolovelace,
  passert,
  passumeWellFormedInlineDatum,
  pcheck,
  pcontainsValue,
  pdatum,
  percentage,
  pfield0,
  pgetCS,
  phasCS,
  pposSingleton,
  ptryFirstJust,
  pvalueOf,
  (#-),
 )
import Vulcan.Utils.Value (phasAsset, ponlySingletonValue)

{- NOTE: almost every parsing (lookup) function here may fail validation.
  Even the functions that return Maybe only return Nothing if the Identity
  Token is not present. They may still fail if the UTxO has the correct Token
  but is malformed in some way.

  Special UTxO inputs assume well-formed datums for performance reasons.

  Special UTxO outputs are not assumed to be well-formed, but some checks
  may be separated from the lookup.
-}

{- | Checks whether UTxO is a @AuctionEscrow@.
  Does it by checking if @AuctionEscrow@ asset present in @Value@.
  Doesn't guarantee that UTxO is well-formed.
  May be used as a lightweight way to find such UTxO for further checking.

  @pisAuction # stateCS # txOut@
-}
pisAuction ::
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PTxOut
        :--> PBool
    )
pisAuction = phoistAcyclic $
  plam $ \stateCS out ->
    let val = pfromData $ pfield @"value" # out
        tn = pnameToken @PAuctionEscrow
     in phasAsset # val # stateCS # tn

{- | Retrieves the datum Auction UTxO from an input UTxO.
  Only checks presence AuctionEscrow token and that the lot is present.
  Fails also found one has lot bought.

  @auctionInputUtxoDatum cfg # stateCS # txOut@
-}
auctionInputUtxoDatum ::
  Vulcan.Config ->
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PAsData PAuctionEscrow)
    )
auctionInputUtxoDatum cfg = phoistAcyclic $
  plam $ \stateCS out -> P.do
    txOut <- pletFields @'["value", "datum"] out
    val <- plet $ pfromData txOut.value
    let tn = pnameToken @PAuctionEscrow
    pcheck (phasAsset # val # stateCS # tn) $ P.do
      datum <- plet $ passumeWellFormedInlineDatum # txOut.datum
      auctionEscrow <- pletFields @'["nodeCS", "terms"] datum
      let lot = pfromData $ pfield @"lot" # auctionEscrow.terms
      passert cfg "Lot is absent" $
        pcontainsValue # val # lot
      datum

{- | Parses a Auction UTxO by ensuring the State Token is contained within the
  value of the TxOut exaclty once, that the datum is of the correct type and
  that the minAda value is correct. As well as ensuring the lot is present.

  Fails if found one has incorrect Value.

  @parseAuctionOutputUtxo cfg # minADAValue # stateCS # txOut@
-}
parseAuctionOutputUtxo ::
  Vulcan.Config ->
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive
        :--> V1.PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PAsData PAuctionEscrow)
    )
parseAuctionOutputUtxo cfg = phoistAcyclic $
  plam $ \minAdaValue stateCS out -> P.do
    txOut <- pletFields @'["value", "datum"] out
    val <- plet $ pfromData txOut.value
    tn <- plet $ pnameToken @PAuctionEscrow
    pcheck (pvalueOf # val # stateCS # tn #== 1) $ P.do
      POutputDatum od <- pmatch . pfromData $ txOut.datum
      datum <- plet $ pdatum #$ pfield @"outputDatum" # od
      auctionEscrow <- pletFields @'["nodeCS", "terms"] datum
      let lot = pfromData $ pfield @"lot" # auctionEscrow.terms
          tokenValue = pposSingleton @1 stateCS tn
          nodeToken = pposSingleton @1 auctionEscrow.nodeCS pcorrNodeTN
          doubleMinAda = minAdaValue <> minAdaValue
      let expectedVal = lot <> tokenValue <> nodeToken <> doubleMinAda
      passert cfg "Incorrect auction value" $
        val #== expectedVal -- Defends from Token Dust attack
      datum

{- Ensures the correct value is held within a bought auction Escrow
  UTxO.

  @checkBoughtUtxoValue # minADAValue # stateCS # nodeCS # txOut@
-}
checkBoughtUtxoValue ::
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive
        :--> V1.PCurrencySymbol
        :--> V1.PCurrencySymbol
        :--> PTxOut
        :--> PBool
    )
checkBoughtUtxoValue = phoistAcyclic $
  plam $ \minAdaValue stateCS nodeCS out ->
    let val = pfromData $ pfield @"value" # out
     in val #== boughtAuctionUtxoValue # minAdaValue # stateCS # nodeCS

{- | Calculates the Value that must have AuctionEscrow UTxO when lot has been bought.

  @boughtAuctionUtxoValue # minADAValue # stateCS # nodeCS@
-}
boughtAuctionUtxoValue ::
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive
        :--> V1.PCurrencySymbol
        :--> V1.PCurrencySymbol
        :--> PValue 'Sorted 'Positive
    )
boughtAuctionUtxoValue = phoistAcyclic $
  plam $ \minAdaValue stateCS nodeCS ->
    let boughtMarkerToken = pposSingleton @1 stateCS boughtTN
        tokenValue = pposSingleton @1 stateCS (pnameToken @PAuctionEscrow)
        nodeToken = pposSingleton @1 nodeCS pcorrNodeTN
        doubleMinAda = minAdaValue <> minAdaValue
     in boughtMarkerToken <> tokenValue <> nodeToken <> doubleMinAda

{- | Retrieves datum from AuctionEscrow input UTxO.
  Only checks presence AuctionEscrow token and that the boughtToken is present

  Fails if found one has no lot bought.

  @boughtAuctionInputUtxoDatum cfg # stateCS # txOut@
-}
boughtAuctionInputUtxoDatum ::
  Vulcan.Config ->
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PAsData PAuctionEscrow)
    )
boughtAuctionInputUtxoDatum cfg = phoistAcyclic $
  plam $ \stateCS out -> P.do
    txOut <- pletFields @'["value", "datum"] out
    value <- plet $ pfromData $ txOut.value
    tn <- plet $ pnameToken @PAuctionEscrow
    pcheck (pvalueOf # value # stateCS # tn #== 1) $ P.do
      datum <- plet $ passumeWellFormedInlineDatum # txOut.datum
      let boughtToken = pposSingleton @1 stateCS boughtTN
      passert cfg "BoughtLot token is absent" $
        pcontainsValue # value # boughtToken
      datum

{- | Retrieves the first AuctionEscrow UTxO datum from a list of resolved inputs.
  Doesn't validate such UTxO, and identifies it by presence of AuctionEscrow token and Lot.

  Fails if  the found Auction UTxO does not hold the lot.

  @auctionDatumFromInputs cfg # stateCS # txOuts@
-}
auctionDatumFromInputs ::
  Vulcan.Config ->
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> PAsData PAuctionEscrow
    )
auctionDatumFromInputs cfg = phoistAcyclic $
  plam $ \stateCS outs ->
    ptryFirstJust # (auctionInputUtxoDatum cfg # stateCS) # outs

{- | Parses the first valid Auction UTxO from a list of TxOuts.

  Fails if no valid Auction UTxO is present.
  Fails if the Value of the found Auction UTxO is incorrect.

  @parseAuctionFromOutputs cfg # minADAValue # stateCS # txOuts@
-}
parseAuctionFromOutputs ::
  Vulcan.Config ->
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive :--> V1.PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> PAsData PAuctionEscrow
    )
parseAuctionFromOutputs cfg = phoistAcyclic $
  plam $ \minAda stateCS outs ->
    ptryFirstJust # (parseAuctionOutputUtxo cfg # minAda # stateCS) # outs

{- | Retrieves the first bought AuctionEscrow UTxO datum from a list of resolved inputs.
  Doesn't validate such UTxO, and identifies it by presence of AuctionEscrow token
  and Bought marker token.

  Fails if the found Auction UTxO does not hold the BoughtLot token.

  @boughtAuctionDatumFromInputs cfg # stateCS # txOuts@
-}
boughtAuctionDatumFromInputs ::
  Vulcan.Config ->
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> PAsData PAuctionEscrow
    )
boughtAuctionDatumFromInputs cfg = phoistAcyclic $
  plam $ \stateCS outs ->
    ptryFirstJust # (boughtAuctionInputUtxoDatum cfg # stateCS) # outs

{- | Retrieves a Bid/Registratrion input UTxO datum.
  It determines such input by presence of it's Identity token asset.
  Validates only that extra value (escrow) is positive.

  Fails if the found UTxO' escrow is not positive.

  @specialInputUtxo tag # minADAValue # stateCS # txOut@
-}
specialInputUtxo ::
  forall (t :: PType).
  ( PTryFrom PData (PAsData t)
  , PIsData t
  , NamedToken t
  ) =>
  SpecialUTxOTag t ->
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive
        :--> V1.PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PUTxO t)
    )
specialInputUtxo tag = phoistAcyclic $
  plam $ \minAdaValue stateCS out -> P.do
    txOut <- pletFields @'["value", "datum"] out
    val <- plet $ pfromData txOut.value
    let tn = pnameToken @t
        tokenValue = pposSingleton @1 stateCS tn
    pcheck (phasAsset # val # stateCS # tn) $
      let datum = passumeWellFormedInlineDatum # txOut.datum
          minVal =
            tokenValue <> minAdaValue <> case tag of
              BidUTxO -> mempty
              RegUTxO -> minAdaValue
          extraValue = passertPositive #$ val #- minVal
       in mkUTxO # datum # extraValue

{- | Parses a Bid/Registratrion UTxO by ensuring the State Token is contained
  within the value of the TxOut exactly once, that the datum is of the
  correct type and that the minAda value is correct.

  Fails if no such UTxO found or if found invalid one.

  Doesn't validate UTxO bid and Value against the auction and market terms.

  @parseSpecOutputUtxo tag # minADAValue # auctionStateTokenValue # txOut@
-}
parseSpecOutputUtxo ::
  forall (t :: PType).
  ( PTryFrom PData (PAsData t)
  , PIsData t
  , NamedToken t
  ) =>
  SpecialUTxOTag t ->
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive
        :--> V1.PValue 'Sorted 'Positive
        :--> PTxOut
        :--> PMaybe (PUTxO t)
    )
parseSpecOutputUtxo tag = phoistAcyclic $
  plam $ \minAdaValue tokenValue out -> P.do
    txOut <- pletFields @'["value", "datum"] out
    val <- plet $ pfromData txOut.value
    -- Prevents any additional tokens being in the Value (Token dust attack)
    pcheck (pnoAdaValue # val #== tokenValue) $ P.do
      POutputDatum od <- pmatch (pfromData $ txOut.datum)
      datum <- plet $ pdatum @t #$ pfield @"outputDatum" # od
      sharedValue <- plet $ tokenValue <> minAdaValue
      let minVal = case tag of
            BidUTxO -> sharedValue
            RegUTxO -> sharedValue <> minAdaValue
          extraValue = passertPositive #$ val #- minVal
      mkUTxO # datum # extraValue

{- | Parses the first BidEscrow UTxO from a list of TxOuts,
  This will fail if no BidEscrow UTxO is present.
  Or if the found UTxO's escrow is not positive.
  Doesn't validate BidEscrow UTxO bid and Value against the auction and market terms.

  @parseBidOutput # minAda # stateCS # outs@
-}
parseBidOutput ::
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive
        :--> V1.PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> PUTxO PBidEscrow
    )
parseBidOutput = phoistAcyclic $
  plam $ \minAda stateCS outs -> P.do
    token <- plet $ ptokenValue @PBidEscrow # stateCS
    ptryFirstJust # (parseSpecOutputUtxo BidUTxO # minAda # token) # outs

{- | Parses the first BidEscrow UTxO from a list of resolved inputs.
  This will fail if no BidEscrow UTxO is present.
   Or if the found UTxO's escrow is not positive.

  @parseBidInput # minAda # stateCS # outs@
-}
parseBidInput ::
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive :--> V1.PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> PUTxO PBidEscrow
    )
parseBidInput = phoistAcyclic $
  plam $ \minAda stateCS outs ->
    ptryFirstJust # (specialInputUtxo BidUTxO # minAda # stateCS) # outs

{- | @bidderFeeFor terms.sellerToCoverFees market.fee.feePercentage bid@
  Calculates bidder fee for a @bid@.

  @bidderFees # sellerToCoverFees # marketTerms # forBid@
-}
bidderFees :: ClosedTerm (PBool :--> PMarketTerms :--> PInteger :--> PInteger)
bidderFees = phoistAcyclic $
  plam $ \sellerToCoverFees marketTerms bid -> P.do
    fee <- pletFields @'["percentageFee", "fixedFee"] marketTerms
    pif sellerToCoverFees 0 $
      (percentage # pfromData fee.percentageFee # bid) + fee.fixedFee

{- | Checks whether BidEscrow UTxO satisfies auction and market terms.

  Ensures a bidUTxO out holds sufficient value to cover the bid and fee as well
   as ensuring that the bid is placed within the correct time frame.

  @
  pvalidBidUtxo
    # utxo -- BidEscrow UTxO to check for validity
    # sellerToCoverFees -- from auction terms
    # startingPrice -- from auction terms
    # marketTerms -- from market UTxO
    # start -- Auction starting time from auction terms
    # close -- Auction starting time from auction terms
    # range -- Tx validity range
  @
-}
pisValidBidUtxo ::
  ClosedTerm
    ( PUTxO PBidEscrow
        :--> PBool
        :--> PPositive
        :--> PMarketTerms
        :--> V1.PPOSIXTime
        :--> V1.PPOSIXTime
        :--> V1.PInterval V1.PPOSIXTime
        :--> PBool
    )
pisValidBidUtxo = plam $
  \utxo sellerToCover startingPrice marketTerms start close range -> P.do
    bidUtxo <- pletFields @'["datum", "extraValue"] utxo
    let status = pfromData $ pfield @"status" # bidUtxo.datum
    pmatch status $ \case
      PNoBid _ -> pcon PTrue
      PBid status' -> P.do
        status <- pletFields @["bid", "time"] status'
        bid <- plet $ pfromData status.bid
        bidLoveLace <- plet $ padaTolovelace # pto bid
        bidTime <- plet $ pfromData status.time
        let bidFeeLovelace = bidderFees # sellerToCover # marketTerms # bidLoveLace
        bidLoveLace + bidFeeLovelace #<= Value.plovelaceValueOf # pfromData bidUtxo.extraValue
          #&& startingPrice #<= bid
          #&& pbefore # start # range
          #&& pafter # bidTime # range
          #&& bidTime #<= close

{- | Retrieves the first RegistrationEscrow input UTxO.
  Fails if no UTxO with RegistrationEscrow token is present.
  Or if the found UTxO's escrow is not positive.

  @registrationInputUtxoDatum # minADAValue # stateCS # txOut@
-}
registrationInputUtxoDatum ::
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive :--> V1.PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> PUTxO PRegistrationEscrow
    )
registrationInputUtxoDatum = phoistAcyclic $
  plam $ \minAda stateCS outs ->
    ptryFirstJust # (specialInputUtxo RegUTxO # minAda # stateCS) # outs

{- | Parses the first valid Registration output UTxO.
   Fails if no UTxO with RegistrationEscrow token is present.
   Or if the found UTxO's escrow is not positive.

  @parseRegOutput # minADAValue # stateCS # outs@
-}
parseRegistrationOutput ::
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive :--> V1.PCurrencySymbol
        :--> PBuiltinList PTxOut
        :--> PUTxO PRegistrationEscrow
    )
parseRegistrationOutput = phoistAcyclic $
  plam $ \minAda stateCS outs -> P.do
    token <- plet $ ptokenValue @PRegistrationEscrow # stateCS
    ptryFirstJust # (parseSpecOutputUtxo RegUTxO # minAda # token) # outs

------------------------------------
-- Market UTxO

{- | Parses a Market UTxO by ensuring that the Market Token is contained within the
  value of the resolved input UTxO.

  @marketInputUtxoDatum # marketCS # outs@
-}
marketInputUtxoDatum ::
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PAsData PMarketTerms)
    )
marketInputUtxoDatum = phoistAcyclic $
  plam $ \marketCS out -> P.do
    txOut <- pletFields @'["value", "datum"] out
    let tn = pnameToken @PMarketTerms
        value = pfromData txOut.value
    pcheck (phasAsset # value # marketCS # tn) $
      passumeWellFormedInlineDatum # txOut.datum

{- | Parses a Market UTxO by ensuring that the Market Token is contained within the
  value of the TxOut exaclty once, that the datum is of the correct type

  @parseMarketInputUtxo # marketTokenValue # txOut@
-}
parseMarketOutputUtxo ::
  ClosedTerm
    ( V1.PValue 'Sorted 'Positive
        :--> PTxOut
        :--> PMaybe (PAsData PMarketTerms)
    )
parseMarketOutputUtxo = phoistAcyclic $
  plam $ \tokenValue out -> P.do
    txOut <- pletFields @'["datum", "value"] out
    let val = pfromData txOut.value
    pcheck (pnoAdaValue # val #== tokenValue) $ P.do
      -- Prevents TokenDust attack
      POutputDatum od <- pmatch (pfromData txOut.datum)
      pdatum #$ pfield @"outputDatum" # od

ptokenValue ::
  forall (n :: PType).
  NamedToken n =>
  ClosedTerm (V1.PCurrencySymbol :--> V1.PValue 'Sorted 'Positive)
ptokenValue = phoistAcyclic $
  plam $ \cs -> pposSingleton @1 cs (pnameToken @n)

----------------------------------------
-- SetNode UTxO

{- | Parses a Node UTxO by ensuring the Currency Symbol matches.
  Does not do additional correctness check, so do not use it
  if UTxO may be invalid (for example, if it's Tx output).
  Use it for Tx inputs, not outputs.
  Lightweight.

  @nodeInputUtxoDatum # nodeCS # txOut@
-}
nodeInputUtxoDatum ::
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PTxOut
        :--> PMaybe (PAsData PSetNode)
    )
nodeInputUtxoDatum = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    txOut <- pletFields @'["datum", "value"] out
    let value = pfromData txOut.value
    pcheck (phasCS # value # nodeCS) $
      passumeWellFormedInlineDatum #$ txOut.datum

{- | Parses a Node UTxO by ensuring the Currency Symbol is contained within the
  value of the TxOut exaclty once, that the datum is of the correct type and
  that the minAda value is correct.

  Returns Nothing if UTxO doesn't contain SetNode token.
  If it does, validation happens and may fail.

  Do not use it for Tx inputs (for performance reasons).

  @parseNodeOutputUtxo cfg # nodeCS # minAda # txOut@
-}
parseNodeOutputUtxo ::
  Vulcan.Config ->
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PPositive
        :--> PTxOut
        :--> PMaybe (PAsData PSetNode)
    )
parseNodeOutputUtxo cfg = phoistAcyclic $
  plam $ \nodeCS minAda out -> P.do
    txOut <- pletFields @'["address", "value", "datum"] out
    value <- plet $ pfromData $ txOut.value
    -- phasCS is one of the lightest (in terms of budget) ways to find UTxO
    pcheck (phasCS # value # nodeCS) $ P.do
      -- Prevents TokenDust attack
      PPair _ token <- pmatch $ ponlySingletonValue cfg #$ pnoAdaValue # value
      let tn = pfromData $ pfstBuiltin # token
          amount = pfromData $ psndBuiltin # token

      POutputDatum od <- pmatch $ pfromData $ txOut.datum
      datum <- plet $ pdatum #$ pfield @"outputDatum" # od
      let nodeKey = pparseNodeKey cfg # tn
          datumKey = pmatch (pfield @"key" # datum) $ \case
            PEmpty _ -> pcon PNothing
            PKey (pfield0 -> key) -> pcon $ PJust key

      passert cfg "Incorrect number of nodeTokens" $ amount #== 1
      passert cfg "node is not ordered" $ validNode # datum
      passert cfg "Incorrect token name" $ nodeKey #== datumKey
      passert cfg "Does not hold minAda" $
        plovelaceValueOf # value #== padaTolovelace # pto minAda
      datum

{- | Extracts the State Token Currency Symbol by finding the UTxO holding the
   corresponding node token and getting the Currency Symbol from the Auction Escrow
   TokenName

  @parseAuctionCS # nodeCS # txOut@
-}
parseAuctionCS ::
  ClosedTerm
    ( V1.PCurrencySymbol
        :--> PTxOut
        :--> PMaybe V1.PCurrencySymbol
    )
parseAuctionCS = phoistAcyclic $
  plam $ \nodeCS out -> P.do
    value <- plet $ pfromData $ pfield @"value" # out
    pif
      (pvalueOf # value # nodeCS # pcorrNodeTN #== 1)
      (pgetCS # value # pnameToken @PAuctionEscrow)
      (pcon PNothing)
