{-# LANGUAGE NegativeLiterals #-}

module Test.Vulcan.Unit.Auction.Private.CancelAct (
  failedAuction,
  mkCtx,
  postBougthAuction,
  cantSpendExtraFromAuction,
  mustBeAuthorizedIfLotPresent,
  cantStealSellerRefund,
  cantMintExtra,
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
  always,
  singleton,
 )

import Test.Vulcan.CommonInputs

data CancelActCtx = MkCancelActCtx
  { marketRefIn :: UTXO
  , auctionIn :: UTXO
  , sellerRefund :: UTXO
  , signatures :: [PubKeyHash]
  , validityRange :: POSIXTimeRange
  , burnAuctionToken :: Value
  , burnHeadNodeToken :: Value
  , burnCorrNodeToken :: Value
  , burnBoughtToken :: Value
  , extraMint :: [Value]
  , extraIns :: [UTXO]
  , extraOuts :: [UTXO]
  }

-- | On failed auction
cantSpendExtraFromAuction :: CancelActCtx -> CancelActCtx
cantSpendExtraFromAuction ctx =
  ctx
    { extraIns =
        [ script privateAuctionHash
        , withValue randomValue
        ]
    }

-- | On failed auction
mustBeAuthorizedIfLotPresent :: CancelActCtx
mustBeAuthorizedIfLotPresent =
  failedAuction {signatures = []}

-- | On failed auction
cantStealSellerRefund :: CancelActCtx -> CancelActCtx
cantStealSellerRefund ctx =
  ctx
    { sellerRefund =
        ctx.sellerRefund
          <> pubKey pk2 -- rewrites UTXO destination because of Last
    }

cantMintExtra :: CancelActCtx -> CancelActCtx
cantMintExtra ctx =
  ctx
    { extraMint = [randomValue]
    }

postBougthAuction :: CancelActCtx
postBougthAuction =
  MkCancelActCtx
    { marketRefIn = marketUtxo
    , auctionIn = auctionUtxo auction LotBought privateAuctionHash
    , sellerRefund =
        mconcat
          [ pubKey pk
          , foldMap
              withValue
              [ minAdaVal
              , minAdaVal
              ]
          ]
    , signatures = []
    , validityRange = always
    , burnAuctionToken = privAuctionToken -1
    , burnHeadNodeToken = nodeToken Nothing -1
    , burnCorrNodeToken = corrNodeToken -1
    , burnBoughtToken = boughtToken Private -1
    , extraMint = []
    , extraIns = []
    , extraOuts = []
    }

failedAuction :: CancelActCtx
failedAuction =
  MkCancelActCtx
    { marketRefIn = marketUtxo
    , auctionIn = auctionUtxo auction LotPresent privateAuctionHash
    , sellerRefund =
        mconcat
          [ pubKey pk
          , foldMap
              withValue
              [ minAdaVal
              , minAdaVal
              , lotVal
              ]
          ]
    , signatures = [sellerPk]
    , validityRange = always
    , burnAuctionToken = privAuctionToken -1
    , burnHeadNodeToken = nodeToken Nothing -1
    , burnCorrNodeToken = corrNodeToken -1
    , burnBoughtToken = mempty
    , extraMint = []
    , extraIns = []
    , extraOuts = []
    }

mkCtx :: CancelActCtx -> ScriptContext
mkCtx cancelAct =
  buildSpending checkVulcan
    . mkNormalized
    $ mconcat
      [ withSpendingOutRef ref1
      , referenceInput cancelAct.marketRefIn
      , foldMap input $ cancelAct.auctionIn : cancelAct.extraIns
      , foldMap output $ cancelAct.sellerRefund : cancelAct.extraOuts
      , foldMap signedWith cancelAct.signatures
      , foldMap
          mint
          [ cancelAct.burnAuctionToken
          , cancelAct.burnCorrNodeToken
          , cancelAct.burnHeadNodeToken
          , cancelAct.burnBoughtToken
          ]
      , foldMap mint cancelAct.extraMint
      , timeRange cancelAct.validityRange
      ]
