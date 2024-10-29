{-# LANGUAGE NegativeLiterals #-}

module Test.Vulcan.Unit.Auction.Private.IssuePaddleAct (
  succeeds,
  mkCtx,
  mustMintBidToken,
  mustNotMintExtra,
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
  withSpendingOutRef,
  withValue,
 )
import PlutusLedgerApi.V2 (
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext,
  Value,
  singleton,
 )

import PlutusLedgerApi.V1.Interval (always, interval, to)
import Test.Vulcan.CommonInputs
import Vulcan.Common.Types.Auction (
  AuctionEscrow (terms),
  AuctionTerms (time),
  AuctionTime (close),
  BidEscrow (status),
  BidStatus (Bid),
 )

registrant :: PubKeyHash
registrant = pk1

data IssuePaddleActCtx = MkIssuePaddleActCtx
  { marketRefIn :: UTXO
  , auctionRefIn :: UTXO
  , registrationIn :: UTXO
  , newBidOut :: UTXO
  , mintBidToken :: Value
  , burnRegistrationToken :: Value
  , mintNodeToken :: Value
  , validityRange :: POSIXTimeRange
  , signatures :: [PubKeyHash]
  , extraMint :: [Value]
  , extraIns :: [UTXO]
  , extraOuts :: [UTXO]
  }

mustMintBidToken :: IssuePaddleActCtx
mustMintBidToken =
  succeeds {mintBidToken = mempty}

mustNotMintExtra :: IssuePaddleActCtx
mustNotMintExtra =
  succeeds {extraMint = [randomValue]}

succeeds :: IssuePaddleActCtx
succeeds =
  MkIssuePaddleActCtx
    { marketRefIn = marketUtxo
    , auctionRefIn = auctionUtxo auction LotPresent privateAuctionHash
    , registrationIn = regEscrowUtxo registration startingVal
    , newBidOut = bidEscrowUtxo bid startingVal privateAuctionHash
    , mintBidToken = privBidToken 1
    , burnRegistrationToken = regToken -1
    , mintNodeToken = nodeToken (Just registrant) 1
    , validityRange = to txValidFrom
    , signatures = [sellerPk]
    , extraMint = []
    , extraIns = []
    , extraOuts = []
    }

mkCtx :: IssuePaddleActCtx -> ScriptContext
mkCtx ctx =
  buildSpending checkVulcan
    . mkNormalized
    $ mconcat
      [ withSpendingOutRef ref1
      , referenceInput ctx.marketRefIn
      , referenceInput ctx.auctionRefIn
      , input ctx.registrationIn
      , output ctx.newBidOut
      , foldMap
          mint
          [ ctx.mintBidToken
          , ctx.burnRegistrationToken
          , ctx.mintNodeToken
          ]
      , timeRange ctx.validityRange
      , foldMap signedWith ctx.signatures
      , foldMap input ctx.extraIns
      , foldMap output ctx.extraOuts
      , foldMap mint ctx.extraMint
      ]
