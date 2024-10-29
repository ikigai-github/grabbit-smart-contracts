{-# LANGUAGE NegativeLiterals #-}

module Test.Vulcan.Unit.Auction.Private.ExtendAct (
  succeeds,
  mkCtx,
  cantMint,
  mustChangeAuction,
  bidTimeMustBeInExtensioWindow,
  doesntSpendExtraFromValidator,
) where

import Plutarch.Context (
  UTXO,
  buildSpending,
  input,
  mint,
  mkNormalized,
  output,
  referenceInput,
  script,
  withSpendingOutRef,
  withValue,
 )
import PlutusLedgerApi.V2 (
  POSIXTimeRange,
  ScriptContext,
  Value,
  singleton,
 )

import PlutusLedgerApi.V1.Interval (interval)
import Test.Vulcan.CommonInputs
import Vulcan.Common.Types.Auction (
  AuctionEscrow (terms),
  AuctionTerms (bidInfo, time),
  AuctionTime (close),
  BidEscrow (status),
  BidInfo (raiseMinimum, startingPrice),
  BidStatus (Bid),
 )

data ExtendActCtx = MkExtendActCtx
  { marketRefIn :: UTXO
  , bidRefIn :: UTXO
  , auctionIn :: UTXO
  , updatedAuctionOut :: UTXO
  , validityInterval :: POSIXTimeRange
  , extraIns :: [UTXO]
  , extraOuts :: [UTXO]
  , extraMint :: Value
  }

mkCtx :: ExtendActCtx -> ScriptContext
mkCtx ctx =
  buildSpending checkVulcan
    . mkNormalized
    $ mconcat
      [ withSpendingOutRef ref1
      , foldMap
          referenceInput
          [ ctx.marketRefIn
          , ctx.bidRefIn
          ]
      , input ctx.auctionIn
      , foldMap input ctx.extraIns
      , output ctx.updatedAuctionOut
      , foldMap output ctx.extraOuts
      , mint ctx.extraMint
      ]

cantMint :: ExtendActCtx
cantMint =
  succeeds
    { extraMint = singleton otherCS "01fc" 3
    }

mustChangeAuction :: ExtendActCtx
mustChangeAuction =
  succeeds
    { updatedAuctionOut = succeeds.auctionIn
    }

bidTimeMustBeInExtensioWindow :: ExtendActCtx
bidTimeMustBeInExtensioWindow =
  succeeds
    { bidRefIn = bidEscrowUtxo extendingBid' mempty privateAuctionHash
    , validityInterval = interval txValidFrom (beforeExtWindow - 1)
    }
  where
    beforeExtWindow = inBetweenOf startTime (lowerExtensionWindowBound - 1)
    extendingBid' =
      extendingBid
        { status = Bid 20 beforeExtWindow
        }

doesntSpendExtraFromValidator :: ExtendActCtx
doesntSpendExtraFromValidator =
  succeeds
    { extraIns =
        [ script privateAuctionHash
            <> withValue (lovelace 5)
        ]
    }

succeeds :: ExtendActCtx
succeeds =
  let ownHash = privateAuctionHash
      updatedTime = auction.terms.time {close = extendedTime}
      updatedBidInfo = auction.terms.bidInfo {startingPrice = 20 + auction.terms.bidInfo.raiseMinimum}
      updatedAuction =
        auction {terms = auction.terms {bidInfo = updatedBidInfo, time = updatedTime}}
   in MkExtendActCtx
        { marketRefIn = marketUtxo
        , bidRefIn = bidEscrowUtxo extendingBid mempty ownHash
        , auctionIn = auctionUtxo auction LotPresent ownHash
        , updatedAuctionOut = auctionUtxo updatedAuction LotPresent ownHash
        , validityInterval = interval txValidFrom (getBidTime extendingBid - 1)
        , extraIns = []
        , extraOuts = []
        , extraMint = mempty
        }
