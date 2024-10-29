{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Test.Vulcan.CommonInputs (
  ------------------------------------
  -- Test Parameters
  exampleTerms,
  exampleBidInfo,
  exampleTimeExtension,
  exampleMarket,
  minimumAda,
  ------------------------------
  -- Validators
  privateAuctionHash,
  publicAuctionHash,
  ------------------------------
  -- Market:
  marketToken,
  marketDatum,
  marketCS,
  marketPK,
  ------------------------------
  -- Nodes:
  separators,
  finSetCS,
  corrNodeToken,
  nodeToken,
  emptyNode,
  firstNode,
  lastNode,
  insertNode,
  updatedNode,
  finSetValHash,
  ---------------------------------
  -- Seps:
  sepToken,
  sepNode1,
  sepNode2,
  sepNode3,
  sepNode4,
  sepNode5,
  incorrectSep,
  sep1,
  sep2,
  sep3,
  sep4,
  sepwithPKNode,
  sepwithPKNode2,
  -- Auction:
  AuctionType (Private, Public),
  AuctionStatus (LotPresent, LotBought),
  privateStateCS,
  publicStateCS,
  privAuctionToken,
  pubAuctionToken,
  incorrectAuctionToken,
  auctionDatum,
  auction,
  auctionDH,
  ---------------------------------
  -- Bid:
  privBidToken,
  pubBidToken,
  bid,
  higherBid,
  raisedBid,
  ---------------------------------
  -- Registration
  regToken,
  registration,
  ---------------------------------
  -- Misc:
  lotVal,
  startTime,
  closeTime,
  cutOffTime,
  minAdaVal,
  doubleMinAdaVal,
  startingVal,
  change,
  price,
  fees,
  halfPrice,
  ---------------------------------
  -- Datums:
  randDatum,
  mkD,
  mkDH,
  ----------------------------------
  -- Hashes and Addresses
  privateValidator,
  randAddress,
  otherValHash,
  pk,
  pk1,
  pk2,
  sellerPk,
  beneficiary1,
  beneficiary2,
  marketAddress,
  correctRef,
  ref1,
  ref2,
  checkVulcan,
  boughtToken,
  addressPkh,
  halfBeneficiariesReward,
  bidEscrowUtxo,
  auctionUtxo,
  marketUtxo,
  beneficiariesReward,
  otherCS,
  marketOref,
  lovelace,
  WhoCoversFees (SellerCovers, WinnerCovers),

  -- * Time
  extendedTime,
  timeInExtensionWindow,
  extendingBid,
  higherTimeInExtensionWindow,
  inBetweenOf,
  txValidFrom,
  lowerExtensionWindowBound,
  updateBidStatus,
  middleOfAuction,
  getBidTime,
  regEscrowUtxo,
  randomValue,
  getBidSize,
  ada,
  justAfter,
  justBefore,
  getBeneficiaryAddress,

  -- * Lenses
  get,
  set,
  _terms,
  _auctionInfo,
  _sellerToCover,
  beneficiary3,
) where

import PlutusLedgerApi.V2 (
  Address (Address),
  BuiltinByteString,
  Credential (PubKeyCredential, ScriptCredential),
  CurrencySymbol,
  Datum (Datum),
  DatumHash,
  POSIXTime,
  PubKeyHash (getPubKeyHash),
  ToData,
  TokenName (TokenName),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
  singleton,
 )
import PlutusTx qualified as Plutus
import PlutusTx.AssocMap (fromList)

import Data.Functor ((<&>))
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Identity (Identity (..))
import Data.String (IsString)
import GHC.Stack (HasCallStack)
import Plutarch.Api.V2 (datumHash)
import Plutarch.Context (Builder, Checker, UTXO, normalizeValue, script, withInlineDatum, withRef, withValue)
import Vulcan.Common.Types.Auction (
  AuctionEscrow (..),
  AuctionInfo (..),
  AuctionTerms (..),
  AuctionTime (..),
  BidEscrow (..),
  BidInfo (..),
  BidStatus (Bid, NoBid),
  MarketTerms (..),
  Positive,
  RegistrationEscrow (MkRegistrationEscrow),
  TimeExtension (..),
 )
import Vulcan.Common.Types.FinSet (
  NodeKey (..),
  SeparatorConfig (..),
  SetNode (..),
  corrNodePrefix',
  setNodePrefix',
 )

------------------------------------
-- Context Builder Parameters
checkVulcan :: Builder a => [Checker e a]
checkVulcan = mempty -- checkPhase1

------------------------------------
-- Test Parameters

exampleTerms :: AuctionTerms
exampleTerms =
  MkAuctionTerms
    { lot = lotVal
    , auctionInfo = exampleAuctionInfo
    , bidInfo = exampleBidInfo
    , time = exampleTime
    }

exampleAuctionInfo :: AuctionInfo
exampleAuctionInfo =
  MkAuctionInfo
    { seller = Address (PubKeyCredential pk) Nothing
    , beneficiaries =
        fromList
          [ beneficiary1
          , beneficiary2
          ]
    , sellerToCoverFees = True
    }

exampleBidInfo :: BidInfo
exampleBidInfo =
  MkBidInfo
    { raiseMinimum = 20
    , raisePercentage = 20
    , startingPrice = 10
    , buyNowPrice = 200
    }

exampleTime :: AuctionTime
exampleTime =
  MkAuctionTime
    { start = startTime
    , close = closeTime
    , extension = Just exampleTimeExtension
    }

exampleTimeExtension :: TimeExtension
exampleTimeExtension =
  MkTimeExtension
    { window = extensionWindow
    , length = extensionLength
    }

exampleMarket :: MarketTerms
exampleMarket =
  MkMarketTerms
    { fixedFee = 2 -- Flat fixed fee
    , percentageFee = 0 -- Fee percentage of the winning Bid
    , feeAddress = marketAddress
    , minAda = minimumAda
    }

minimumAda :: Positive
minimumAda = 2

------------------------------
-- Validators/MP
-- TODO randomise these

finSetCS :: CurrencySymbol
finSetCS = "9b7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a"

privateStateCS :: CurrencySymbol
privateStateCS = "8c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a"

publicStateCS :: CurrencySymbol
publicStateCS = "7c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a"

privateAuctionHash :: ValidatorHash
privateAuctionHash = "6c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a"

publicAuctionHash :: ValidatorHash
publicAuctionHash = "5c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a"

finSetValHash :: ValidatorHash
finSetValHash = "4c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a"

------------------------------
-- Market:

marketToken :: Value
marketToken = singleton marketCS "MarketEscrow" 1

marketDatum :: Datum
marketDatum = Datum $ Plutus.toBuiltinData exampleMarket

marketCS :: CurrencySymbol
marketCS = "9b8bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a"

marketPK :: PubKeyHash
marketPK = "adfd87319bd09c9e3ea10b251ccb046f87c5440343157e348c3ac7c1"

marketAddress :: Address
marketAddress = Address (PubKeyCredential marketPK) Nothing

------------------------------
-- Nodes:

separators :: SeparatorConfig
separators = MkSeperatorConfig pk cutOffTime

nodeToken :: Maybe PubKeyHash -> Integer -> Value
nodeToken pkh amount =
  case pkh of
    Nothing -> singleton finSetCS (TokenName setNodePrefix') amount
    Just pkh' -> singleton finSetCS (TokenName $ setNodePrefix' <> getPubKeyHash pkh') amount

sepToken :: BuiltinByteString -> Integer -> Value
sepToken bs = singleton finSetCS (TokenName $ setNodePrefix' <> makeSep bs)

corrNodeToken :: Integer -> Value
corrNodeToken = singleton finSetCS (TokenName corrNodePrefix')

emptyNode :: SetNode
emptyNode = MkSetNode Empty Empty

firstNode :: SetNode
firstNode = MkSetNode Empty (Key $ getPubKeyHash pk1)

lastNode :: SetNode
lastNode = MkSetNode (Key $ getPubKeyHash pk1) Empty

insertNode :: SetNode
insertNode = MkSetNode (Key $ getPubKeyHash pk) (Key $ getPubKeyHash pk1)

updatedNode :: SetNode
updatedNode = MkSetNode Empty (Key $ getPubKeyHash pk)

sepwithPKNode :: SetNode
sepwithPKNode = MkSetNode (Key $ makeSep sep1) (Key $ getPubKeyHash pk1)

sepwithPKNode2 :: SetNode
sepwithPKNode2 = MkSetNode (Key $ getPubKeyHash pk1) (Key $ makeSep sep2)

sepNode1 :: SetNode
sepNode1 = MkSetNode Empty (Key $ makeSep sep1)

sepNode2 :: SetNode
sepNode2 = MkSetNode (Key $ makeSep sep1) (Key $ makeSep sep2)

sepNode3 :: SetNode
sepNode3 = MkSetNode (Key $ makeSep sep2) (Key $ makeSep sep3)

sepNode4 :: SetNode
sepNode4 = MkSetNode (Key $ makeSep sep3) (Key $ makeSep sep4)

sepNode5 :: SetNode
sepNode5 = MkSetNode (Key $ makeSep sep4) Empty

incorrectSep :: SetNode
incorrectSep = MkSetNode (Key $ makeSep sep4) (Key $ makeSep sep3)

makeSep :: BuiltinByteString -> BuiltinByteString
makeSep n = n <> "Sep"

sep1 :: BuiltinByteString
sep1 = "66666"
sep2 :: BuiltinByteString
sep2 = "99999"
sep3 :: BuiltinByteString
sep3 = "ccccc"
sep4 :: BuiltinByteString
sep4 = "nnnnn"

---------------------------------
-- UTxOs

marketUtxo :: UTXO
marketUtxo =
  mconcat
    [ script otherValHash
    , withValue $ minAdaVal <> marketToken
    , withInlineDatum $ mkD exampleMarket
    ]

auctionUtxo ::
  AuctionEscrow ->
  AuctionStatus ->
  ValidatorHash ->
  UTXO
auctionUtxo auction status auctionValidator =
  let datum = Datum $ Plutus.toBuiltinData auction
   in mconcat
        [ script auctionValidator
        , foldMap
            withValue
            [ privAuctionToken 1
            , minAdaVal <> minAdaVal
            , corrNodeToken 1
            , case status of
                LotBought -> boughtToken Private 1
                LotPresent -> auction.terms.lot
            ]
        , withInlineDatum datum
        , withRef ref1
        ]
regEscrowUtxo ::
  RegistrationEscrow ->
  Value ->
  UTXO
regEscrowUtxo regEscrow escrow =
  mconcat
    [ script privateAuctionHash
    , foldMap
        withValue
        [ escrow
        , minAdaVal <> minAdaVal
        , regToken 1
        ]
    , withInlineDatum $ mkD regEscrow
    , withRef ref1
    ]

bidEscrowUtxo ::
  BidEscrow ->
  Value ->
  ValidatorHash ->
  UTXO
bidEscrowUtxo bid extra auctionValidator =
  let bidAmount = case bid.status of
        Bid amount _ -> amount
        NoBid -> 0
   in mconcat
        [ script auctionValidator
        , withValue . normalizeValue $
            mconcat
              [ minAdaVal
              , ada bidAmount
              , privBidToken 1
              , extra
              ]
        , withInlineDatum $ mkD bid
        , withRef ref1
        ]

---------------------------------
-- Auction:

privAuctionToken :: Integer -> Value
privAuctionToken = singleton privateStateCS "AuctionEscrow"

pubAuctionToken :: Integer -> Value
pubAuctionToken = singleton publicStateCS "AuctionEscrow"

incorrectAuctionToken :: Value
incorrectAuctionToken = singleton "c3" "AuctionEscrow" 1

auction :: AuctionEscrow
auction = MkAuctionEscrow {terms = exampleTerms, nodeCS = finSetCS}

auctionDatum :: Datum
auctionDatum = Datum $ Plutus.toBuiltinData auction

auctionDH :: DatumHash
auctionDH = datumHash auctionDatum

---------------------------------
-- Bid:

regToken :: Integer -> Value
regToken = singleton privateStateCS "RegistrationEscrow"

privBidToken :: Integer -> Value
privBidToken = singleton privateStateCS "BidEscrow"

pubBidToken :: Integer -> Value
pubBidToken = singleton publicStateCS "BidEscrow"

boughtToken :: AuctionType -> Integer -> Value
boughtToken = \case
  Private -> singleton privateStateCS "BoughtEscrow"
  Public -> singleton publicStateCS "BoughtEscrow"

data AuctionType = Private | Public
data AuctionStatus = LotBought | LotPresent

bid :: BidEscrow
bid =
  MkBidEscrow
    { status = NoBid
    , bidder = Address (PubKeyCredential pk1) Nothing
    }

extendingBid :: BidEscrow
extendingBid =
  MkBidEscrow
    { status = Bid 20 timeInExtensionWindow
    , bidder = Address (PubKeyCredential pk2) Nothing
    }

higherBid :: BidEscrow
higherBid =
  MkBidEscrow
    { status = Bid 25 1_500_000
    , bidder = Address (PubKeyCredential pk2) Nothing
    }

raisedBid :: BidEscrow
raisedBid =
  MkBidEscrow
    { status = Bid 20 (inBetweenOf txValidFrom middleOfAuction)
    , bidder = Address (PubKeyCredential pk1) Nothing
    }

getBidTime :: HasCallStack => BidEscrow -> POSIXTime
getBidTime MkBidEscrow {status = Bid _ time} = time

getBidSize :: HasCallStack => BidEscrow -> Integer
getBidSize MkBidEscrow {status = Bid size _} = size

updateBidStatus ::
  (Integer -> Integer) ->
  (POSIXTime -> POSIXTime) ->
  BidEscrow ->
  BidEscrow
updateBidStatus overBid overBidTime bid =
  case bid.status of
    NoBid -> bid
    Bid amount time -> bid {status = Bid (overBid amount) (overBidTime time)}

---------------------------------
-- Registration

registration :: RegistrationEscrow
registration =
  MkRegistrationEscrow $
    Address (PubKeyCredential pk1) Nothing

---------------------------------
-- Misc:

nftCS :: CurrencySymbol
nftCS = "1865710ee3b580f02787f3e5830f78ef6e23ed75cf53c304dfce562a"

nftTN :: TokenName
nftTN = "NFT"

lotVal :: Value
lotVal = singleton nftCS nftTN 1

randomValue :: Value
randomValue = singleton otherCS "45faeb" 3

startTime :: POSIXTime
startTime = 1_000_000

cutOffTime :: POSIXTime
cutOffTime = 1_500_000

closeTime :: POSIXTime
closeTime = 2_000_000

txValidFrom :: POSIXTime
txValidFrom = justAfter startTime

lowerExtensionWindowBound :: POSIXTime
lowerExtensionWindowBound =
  closeTime - fromIntegral exampleTimeExtension.window

middleOfAuction :: POSIXTime
middleOfAuction = inBetweenOf txValidFrom closeTime

inBetweenOf :: HasCallStack => Integral n => n -> n -> n
inBetweenOf min max
  | half /= 0 =
      min + half
  | otherwise = error "Can't fit number in between"
  where
    half = (max - min) `div` 2

extensionWindowTime :: POSIXTime
extensionWindowTime = closeTime + fromIntegral extensionWindow

timeInExtensionWindow :: POSIXTime
timeInExtensionWindow =
  inBetweenOf extensionWindowTime closeTime

higherTimeInExtensionWindow :: POSIXTime
higherTimeInExtensionWindow =
  inBetweenOf timeInExtensionWindow closeTime

extendedTime :: POSIXTime
extendedTime = closeTime + fromIntegral extensionLength

extensionWindow :: Integer
extensionWindow = 1_000

extensionLength :: Integer
extensionLength = 2_000

minAdaVal :: Value
minAdaVal = lovelace 2_000_000

doubleMinAdaVal :: Value
doubleMinAdaVal = lovelace 4_000_000

startingVal :: Value
startingVal = lovelace 30_000_000

change :: Value
change = lovelace 10_000_000

price :: Value
price = lovelace 22_000_000

data WhoCoversFees = SellerCovers | WinnerCovers
  deriving stock (Eq)

-- | In lovelace
beneficiariesReward :: WhoCoversFees -> Integer
beneficiariesReward SellerCovers = 202_000_000
beneficiariesReward WinnerCovers =
  beneficiariesReward SellerCovers - fees

-- | In lovelace
halfBeneficiariesReward :: WhoCoversFees -> Integer
halfBeneficiariesReward whoCovers =
  beneficiariesReward whoCovers `div` 2

-- | In lovelace
fees :: Integer
fees = 2_000_000

halfPrice :: Value
halfPrice = lovelace 11_000_000

---------------------------------
-- Datums:

randDatum :: Datum
randDatum = Datum $ Plutus.toBuiltinData (1 :: Integer)

mkD :: ToData a => a -> Datum
mkD = Datum . Plutus.toBuiltinData

mkDH :: ToData a => a -> DatumHash
mkDH = datumHash . Datum . Plutus.toBuiltinData

----------------------------------
-- Hashes and Addresses

addressPkh :: Address -> Maybe PubKeyHash
addressPkh (Address pkCredential _) =
  case pkCredential of
    PubKeyCredential pkh -> Just pkh
    _ -> Nothing

privateValidator :: Address
privateValidator = Address (ScriptCredential privateAuctionHash) Nothing

randAddress :: Address
randAddress = Address (PubKeyCredential pk2) Nothing

otherValHash :: ValidatorHash
otherValHash = random28BS

otherCS :: CurrencySymbol
otherCS = random28BS

random28BS :: forall (s :: Type). IsString s => s
random28BS = "7c7bfa6b888fb3e600d4d9505b4fbca905d23ac23ed623b7170ab12a"

sellerPk :: PubKeyHash
sellerPk = pk

pk :: PubKeyHash
pk = "7c7bfa6b888fb3e600d4d9505b4fbca905df8ac58ed623b7170ab12a"

pk1 :: PubKeyHash
pk1 = "8c7bfa6b888fb3e600d4d9505b4fbca90574867967898798657ef6af"

pk2 :: PubKeyHash
pk2 = "97bfa6b998fb3e600d4d5656567567889080980989808080809890ee"

beneficiary1 :: (Address, Integer)
beneficiary1 = (Address (PubKeyCredential pk) Nothing, 50)

beneficiary2 :: (Address, Integer)
beneficiary2 = (Address (PubKeyCredential pk1) Nothing, 50)

beneficiary3 :: (Address, Integer)
beneficiary3 = (Address (PubKeyCredential pk2) Nothing, 50)

correctRef :: TxOutRef
correctRef = TxOutRef "b2" 1

ref1 :: TxOutRef
ref1 = TxOutRef "a0" 0

ref2 :: TxOutRef
ref2 = TxOutRef "c0" 0

marketOref :: TxOutRef
marketOref = TxOutRef "7865710ee3b580f02787f3e5830f78ef6e23ed75cf53c304dfce562a" 1

ada :: Integer -> Value
ada = lovelace . (* 10 ^ 6)

lovelace :: Integer -> Value
lovelace = singleton "" ""

justBefore :: POSIXTime -> POSIXTime
justBefore = subtract 1

justAfter :: POSIXTime -> POSIXTime
justAfter = (+ 1)

getBeneficiaryAddress :: forall {b}. (Address, b) -> PubKeyHash
getBeneficiaryAddress (addr, _) =
  case addressPkh addr of
    Just pk -> pk
    Nothing -> error "Can't extract address pkh"

-- * Lenses

-- | Needs to painlessly make tests for alternative terms
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

get :: forall s t a b. Lens s t a b -> s -> a
get lens = getConst . lens Const

set :: forall s t a b. Lens s t a b -> b -> s -> t
set lens x = runIdentity . lens (Identity . const x)

_terms :: Lens' AuctionEscrow AuctionTerms
_terms mapTerms auction =
  mapTerms auction.terms <&> \terms -> auction {terms = terms}

_auctionInfo :: Lens' AuctionTerms AuctionInfo
_auctionInfo mapInfo terms =
  mapInfo terms.auctionInfo <&> \info -> terms {auctionInfo = info}

_sellerToCover :: Lens' AuctionInfo Bool
_sellerToCover mapField info =
  mapField info.sellerToCoverFees <&> \b -> info {sellerToCoverFees = b}
