{-# LANGUAGE NegativeLiterals #-}

module Test.Vulcan.Unit.Auction.Private.ResolveAct (
  mkCtx,
  whenSellerPayingFees,
  whenBidderPayingFees,
  mustCorrectlyPayBeneficiaries,
  mustPayMarketFees,
  mustBurnAuction,
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
 )

import PlutusTx.Numeric qualified as Numeric
import Test.Vulcan.CommonInputs

data ResolveActCtx = MkResolveActCtx
  { marketRefIn :: UTXO
  , auctionIn :: UTXO
  , winningBidIn :: UTXO
  , winnerReward :: UTXO
  , toBeneficiary1 :: UTXO
  , toBeneficiary2 :: UTXO
  , marketFees :: UTXO
  , burnAuctionEscrowToken :: Value
  , burnHeadNodeToken :: Value
  , burnCorrNodeToken :: Value
  , burnBidEscrowToken :: Value
  , burnBidNodeToken :: Value
  , validityRange :: POSIXTimeRange
  , extraMint :: [Value]
  , extraIns :: [UTXO]
  , extraOuts :: [UTXO]
  }
  deriving stock (Show)

winnerPk :: PubKeyHash
winnerPk = pk2

-- * Invalid contexts

mustBurnAuction :: ResolveActCtx -> ResolveActCtx
mustBurnAuction ctx =
  ctx {burnAuctionEscrowToken = mempty}

mustPayMarketFees :: ResolveActCtx -> ResolveActCtx
mustPayMarketFees ctx =
  ctx {marketFees = mempty}

mustCorrectlyPayBeneficiaries :: ResolveActCtx -> ResolveActCtx
mustCorrectlyPayBeneficiaries ctx =
  ctx
    { toBeneficiary1 =
        ctx.toBeneficiary1 <> withValue (Numeric.negate minAdaVal)
    , toBeneficiary2 =
        ctx.toBeneficiary1 <> withValue (Numeric.negate minAdaVal)
    }

-- * Valid contexts

whenBidderPayingFees :: ResolveActCtx
whenBidderPayingFees =
  whenSellerPayingFees
    { auctionIn =
        auctionUtxo (winnerToCoverFees auction) LotPresent privateAuctionHash
          <> withRef ref1
    , winnerReward =
        whenSellerPayingFees.winnerReward
          <> withValue (lovelace $ negate fees)
    , toBeneficiary1 =
        mconcat
          [ pubKey $ getBeneficiaryAddress beneficiary1
          , withValue $ lovelace $ halfBeneficiariesReward WinnerCovers
          ]
    , toBeneficiary2 =
        mconcat
          [ pubKey $ getBeneficiaryAddress beneficiary2
          , withValue $ lovelace $ halfBeneficiariesReward WinnerCovers
          ]
    }
  where
    winnerToCoverFees =
      (_terms . _auctionInfo . _sellerToCover) `set` False

whenSellerPayingFees :: ResolveActCtx
whenSellerPayingFees =
  MkResolveActCtx
    { marketRefIn = marketUtxo
    , auctionIn =
        auctionUtxo auction LotPresent privateAuctionHash
          <> withRef ref1
    , winningBidIn =
        bidEscrowUtxo higherBid startingVal privateAuctionHash
          <> withRef ref1
    , winnerReward =
        mconcat
          [ pubKey winnerPk
          , withValue $ startingVal <> lotVal <> minAdaVal
          ]
    , toBeneficiary1 =
        mconcat
          [ pubKey $ getBeneficiaryAddress beneficiary1
          , withValue $ lovelace $ halfBeneficiariesReward SellerCovers
          ]
    , toBeneficiary2 =
        mconcat
          [ pubKey $ getBeneficiaryAddress beneficiary2
          , withValue $ lovelace $ halfBeneficiariesReward SellerCovers
          ]
    , marketFees = pubKey marketPK <> withValue (lovelace fees)
    , burnAuctionEscrowToken = privAuctionToken -1
    , burnHeadNodeToken = nodeToken Nothing -1
    , burnCorrNodeToken = corrNodeToken -1
    , burnBidEscrowToken = privBidToken -1
    , burnBidNodeToken = nodeToken (Just winnerPk) -1
    , validityRange = from $ justAfter closeTime
    , extraMint = []
    , extraIns = []
    , extraOuts = []
    }

mkCtx :: ResolveActCtx -> ScriptContext
mkCtx ctx =
  buildSpending checkVulcan
    . mkNormalized
    $ mconcat
      [ withSpendingOutRef ref1
      , referenceInput ctx.marketRefIn
      , foldMap
          input
          [ ctx.auctionIn
          , ctx.winningBidIn
          ]
      , foldMap
          output
          [ ctx.winnerReward
          , ctx.toBeneficiary1
          , ctx.toBeneficiary2
          , ctx.marketFees
          ]
      , mconcat
          [ -- StateMP mint
            mint $
              mconcat
                [ ctx.burnAuctionEscrowToken
                , ctx.burnBidEscrowToken
                ]
          , -- FinSet mint
            mint $
              mconcat
                [ ctx.burnBidNodeToken
                , ctx.burnHeadNodeToken
                , ctx.burnCorrNodeToken
                ]
          ]
      , timeRange ctx.validityRange
      , foldMap input ctx.extraIns
      , foldMap output ctx.extraOuts
      , foldMap mint ctx.extraMint
      ]
