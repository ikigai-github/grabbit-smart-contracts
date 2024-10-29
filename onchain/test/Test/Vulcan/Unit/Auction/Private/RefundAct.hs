{-# LANGUAGE NegativeLiterals #-}

module Test.Vulcan.Unit.Auction.Private.RefundAct (
  mkCtx,

  -- * Valid contexts
  againstHigherAfterAuction,
  whenNoBidAfterAuction,
  noBidWhenLotBought,

  -- * Invalid contexts contexts
  mustRefundCorrectBidder,
  mustNotRefundBidDuringAuctionUnauthorized,
  mustNotMintExtra,
  mustRefundMinAda,

  -- * Partial contexts and transformations
  theBid,
  noBid,
  refundOneMinAdaLess,
  buyNowRefundMustNotWorkWhenLotPresent,
  duringAuctionWhenAuthorized,
  afterAuctionWhenUnauthorized,
) where

import Plutarch.Context (
  UTXO,
  buildSpending,
  input,
  mint,
  mkNormalized,
  output,
  pubKey,
  referenceInput,
  script,
  signedWith,
  timeRange,
  withRef,
  withSpendingOutRef,
  withValue,
 )
import PlutusLedgerApi.V2 (
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext,
  Value,
  from,
  singleton,
 )

import PlutusLedgerApi.V1.Interval (always, interval, never, to)
import PlutusTx.Numeric (Module (scale))
import Test.Vulcan.CommonInputs
import Vulcan.Common.Types.Auction (
  AuctionEscrow (terms),
  AuctionTerms (time),
  AuctionTime (close),
  BidEscrow (status),
  BidStatus (Bid),
 )

data RefundActCtx = MkRefundActCtx
  { marketRefIn :: UTXO
  , auctionRefIn :: UTXO
  , higherBidRef :: UTXO
  -- ^ need only when refund non-empty lost bid
  , bidToRefund :: UTXO
  , refund :: UTXO
  , burnBidEscrowToken :: Value
  , burnBidderNodeToken :: Value
  , validityRange :: POSIXTimeRange
  , signatures :: [PubKeyHash]
  , extraMint :: [Value]
  , extraIns :: [UTXO]
  , extraOuts :: [UTXO]
  }
  deriving (Show)

bidderPk :: PubKeyHash
bidderPk = pk1

-- * Negative

mustNotMintExtra :: RefundActCtx -> RefundActCtx
mustNotMintExtra ctx =
  ctx {extraMint = [randomValue]}

mustRefundMinAda :: RefundActCtx -> RefundActCtx
mustRefundMinAda = refundOneMinAdaLess . refundOneMinAdaLess

buyNowRefundMustNotWorkWhenLotPresent :: RefundActCtx -> RefundActCtx
buyNowRefundMustNotWorkWhenLotPresent ctx =
  ctx {auctionRefIn = auctionUtxo auction LotPresent privateAuctionHash}

mustRefundCorrectBidder :: RefundActCtx -> RefundActCtx
mustRefundCorrectBidder ctx =
  ctx {refund = ctx.refund <> pubKey pk2}

mustNotRefundBidDuringAuctionUnauthorized :: RefundActCtx -> RefundActCtx
mustNotRefundBidDuringAuctionUnauthorized ctx =
  (duringAuctionWhenAuthorized ctx) {signatures = []}

-- * Positive

againstHigherAfterAuction :: RefundActCtx
againstHigherAfterAuction =
  afterAuctionWhenUnauthorized $
    theBid
      common
        { higherBidRef =
            bidEscrowUtxo higherBid mempty privateAuctionHash
        }

whenNoBidAfterAuction :: RefundActCtx
whenNoBidAfterAuction =
  noBid $ afterAuctionWhenUnauthorized common

noBidWhenLotBought :: RefundActCtx
noBidWhenLotBought =
  noBid $
    common
      { auctionRefIn = auctionUtxo auction LotBought privateAuctionHash
      , validityRange = always
      }

-- * Partial contexts

refundOneMinAdaLess :: RefundActCtx -> RefundActCtx
refundOneMinAdaLess ctx =
  ctx {refund = ctx.refund <> withValue (negAmountOf minAdaVal)}
  where
    negAmountOf = scale -1

duringAuctionWhenAuthorized :: RefundActCtx -> RefundActCtx
duringAuctionWhenAuthorized ctx =
  ctx
    { validityRange = interval txValidFrom middleOfAuction
    , signatures = [bidderPk]
    }

afterAuctionWhenUnauthorized :: RefundActCtx -> RefundActCtx
afterAuctionWhenUnauthorized ctx =
  ctx
    { validityRange = from $ justAfter closeTime
    , signatures = []
    }

theBid :: RefundActCtx -> RefundActCtx
theBid ctx =
  ctx
    { bidToRefund =
        bidEscrowUtxo raisedBid mempty privateAuctionHash
    , refund =
        mconcat
          [ pubKey bidderPk
          , foldMap
              withValue
              [ ada $ getBidSize raisedBid
              , minAdaVal -- from BidEscrow
              , minAdaVal
              {- SetNode minADA not necessary goes to the bidder,
                the one who submitted Tx is free to take it or
                pay Tx fees from it
              -}
              ]
          ]
    }

noBid :: RefundActCtx -> RefundActCtx
noBid ctx =
  ctx
    { bidToRefund =
        bidEscrowUtxo bid startingVal privateAuctionHash
    , refund =
        mconcat
          [ pubKey bidderPk
          , foldMap
              withValue
              [ startingVal
              , minAdaVal <> minAdaVal
              {- SetNode minADA not necessary goes to the bidder,
                the one who submitted Tx is free to take it or
                pay Tx fees from it
              -}
              ]
          ]
    }

-- | Shared between contexts, incomplete by itself
common :: RefundActCtx
common =
  MkRefundActCtx
    { marketRefIn = marketUtxo
    , auctionRefIn =
        auctionUtxo auction LotPresent privateAuctionHash
    , higherBidRef = mempty -- placeholder
    , bidToRefund = mempty -- placeholder
    , refund = mempty -- placeholder
    , burnBidEscrowToken = privBidToken -1
    , burnBidderNodeToken = nodeToken (Just bidderPk) -1
    , validityRange = never -- placeholder
    , signatures = [] -- placeholder
    , extraMint = []
    , extraIns = []
    , extraOuts = []
    }

mkCtx :: RefundActCtx -> ScriptContext
mkCtx ctx =
  buildSpending checkVulcan
    . mkNormalized
    $ mconcat
      [ withSpendingOutRef ref1
      , foldMap
          referenceInput
          [ ctx.marketRefIn
          , ctx.auctionRefIn
          , ctx.higherBidRef
          ]
      , input ctx.bidToRefund
      , output ctx.refund
      , foldMap
          mint
          [ ctx.burnBidEscrowToken
          , ctx.burnBidderNodeToken
          ]
      , timeRange ctx.validityRange
      , foldMap signedWith ctx.signatures
      , foldMap input ctx.extraIns
      , foldMap output ctx.extraOuts
      , foldMap mint ctx.extraMint
      ]
