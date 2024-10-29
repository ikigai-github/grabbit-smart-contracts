{-# LANGUAGE NegativeLiterals #-}

module Test.Vulcan.Unit.Auction.Private.BidAct (
  succeeds,
  mkCtx,
  mustNotMint,
  mustNotMintNode,
  bidMustBeAfterValidityRange,
) where

import Plutarch.Context (
  UTXO,
  buildSpending,
  input,
  mint,
  mkNormalized,
  output,
  referenceInput,
  signedWith,
  timeRange,
  withSpendingOutRef,
 )
import PlutusLedgerApi.V2 (
  POSIXTimeRange,
  PubKeyHash,
  ScriptContext,
  Value,
  singleton,
 )

import PlutusLedgerApi.V1.Interval (interval)
import Test.Vulcan.CommonInputs
import Vulcan.Common.Types.Auction (
  BidEscrow (bidder),
 )

data BuyNowCtx = MkBuyNowCtx
  { marketRefIn :: UTXO
  , auctionRefIn :: UTXO
  , bidIn :: UTXO
  , bidOut :: UTXO
  , signatures :: [PubKeyHash]
  , validityRange :: POSIXTimeRange
  , extraMint :: [Value]
  , extraIns :: [UTXO]
  , extraOuts :: [UTXO]
  }

bidMustBeAfterValidityRange :: BuyNowCtx
bidMustBeAfterValidityRange =
  let illegallyRaised =
        updateBidStatus
          id
          (const $ inBetweenOf txValidFrom $ getBidTime raisedBid)
          -- it's time after Tx validity range but in auction time
          raisedBid
      malformedBid = bidEscrowUtxo illegallyRaised mempty privateAuctionHash
   in succeeds {bidOut = malformedBid}

mustNotMintNode :: BuyNowCtx
mustNotMintNode =
  succeeds
    { extraMint = [nodeToken (Just pk1) 1]
    }

mustNotMint :: BuyNowCtx
mustNotMint =
  succeeds {extraMint = [singleton otherCS "25be06" 1]}

succeeds :: BuyNowCtx
succeeds =
  let ownHash = privateAuctionHash
      Just bidderPkh = addressPkh bid.bidder
   in MkBuyNowCtx
        { marketRefIn = marketUtxo
        , auctionRefIn = auctionUtxo auction LotPresent ownHash
        , bidIn = bidEscrowUtxo bid mempty ownHash
        , bidOut = bidEscrowUtxo raisedBid mempty ownHash
        , signatures = [bidderPkh]
        , validityRange = interval txValidFrom (getBidTime raisedBid - 1)
        , extraMint = mempty
        , extraIns = []
        , extraOuts = []
        }

mkCtx :: BuyNowCtx -> ScriptContext
mkCtx bidAct =
  buildSpending checkVulcan
    . mkNormalized
    $ mconcat
      [ withSpendingOutRef ref1
      , foldMap input $ bidAct.bidIn : bidAct.extraIns
      , foldMap output $ bidAct.bidOut : bidAct.extraOuts
      , timeRange bidAct.validityRange
      , foldMap signedWith bidAct.signatures
      , referenceInput bidAct.marketRefIn
      , referenceInput bidAct.auctionRefIn
      , foldMap mint bidAct.extraMint
      ]
