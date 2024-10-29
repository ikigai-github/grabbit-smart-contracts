{-# LANGUAGE NegativeLiterals #-}

module Test.Vulcan.Unit.Auction.Private.UnregisterAct (
  succeeds,
  mkCtx,
  shouldRefundAllFunds,
  shouldntMintExtra,
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

import PlutusLedgerApi.V1.Interval (always, interval)
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

data UnregisterActCtx = MkUnregisterActCtx
  { marketRefIn :: UTXO
  , registrationIn :: UTXO
  , registrantRefund :: UTXO
  , burnRegistrationToken :: Value
  , signatures :: [PubKeyHash]
  , extraMint :: [Value]
  , extraIns :: [UTXO]
  , extraOuts :: [UTXO]
  }

shouldntMintExtra :: UnregisterActCtx
shouldntMintExtra =
  succeeds {extraMint = [randomValue]}

shouldRefundAllFunds :: UnregisterActCtx
shouldRefundAllFunds =
  succeeds
    { registrantRefund = pubKey registrant <> withValue minAdaVal
    }

succeeds :: UnregisterActCtx
succeeds =
  MkUnregisterActCtx
    { marketRefIn = marketUtxo
    , registrationIn = regEscrowUtxo registration startingVal
    , registrantRefund =
        mconcat
          [ pubKey registrant
          , withValue startingVal
          , withValue $ minAdaVal <> minAdaVal
          ]
    , burnRegistrationToken = regToken -1
    , signatures = [registrant]
    , extraMint = []
    , extraIns = []
    , extraOuts = []
    }

mkCtx :: UnregisterActCtx -> ScriptContext
mkCtx ctx =
  buildSpending checkVulcan
    . mkNormalized
    $ mconcat
      [ withSpendingOutRef ref1
      , referenceInput ctx.marketRefIn
      , input ctx.registrationIn
      , output ctx.registrantRefund
      , mint ctx.burnRegistrationToken
      , foldMap signedWith ctx.signatures
      , timeRange always
      , foldMap input ctx.extraIns
      , foldMap output ctx.extraOuts
      , foldMap mint ctx.extraMint
      ]
