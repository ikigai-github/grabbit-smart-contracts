{-# LANGUAGE NegativeLiterals #-}

module Test.Vulcan.Unit.Auction.Private.BuyNowAct (
  succeeds,
  mkCtx,
  mustRewardWinner,
  cantMintMoreBoughtTokens,
  cantMintExtra,
  mustChangeAuction,
) where

import Plutarch.Context (
  UTXO,
  address,
  buildSpending,
  input,
  mint,
  mkNormalized,
  normalizeValue,
  output,
  referenceInput,
  signedWith,
  withRef,
  withSpendingOutRef,
  withValue,
 )
import PlutusLedgerApi.V2 (
  PubKeyHash,
  ScriptContext,
  Value,
  singleton,
 )

import Test.Vulcan.CommonInputs
import Vulcan.Common.Types.Auction (
  AuctionEscrow (terms),
  AuctionTerms (lot),
  BidEscrow (bidder, status),
  BidInfo (buyNowPrice),
  BidStatus (Bid),
 )

mkCtx :: BuyNowCtx -> ScriptContext
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
      , foldMap output ctx.extraIns
      , foldMap
          output
          [ ctx.auctionOut
          , ctx.winnerReward
          , ctx.beneficiary1Reward
          , ctx.beneficiary2Reward
          , ctx.marketFees
          ]
      , foldMap output ctx.extraOuts
      , foldMap
          mint
          [ ctx.burnBidToken
          , ctx.burnBidNodeToken
          , ctx.mintBoughAuctionToken
          , ctx.extraMint
          ]
      , foldMap signedWith ctx.signatures
      ]

data BuyNowCtx = MkBuyNowCtx
  { marketRefIn :: UTXO
  , auctionIn :: UTXO
  , winningBidIn :: UTXO
  , winnerReward :: UTXO
  , beneficiary1Reward :: UTXO
  , beneficiary2Reward :: UTXO
  , auctionOut :: UTXO
  , marketFees :: UTXO
  , burnBidNodeToken :: Value
  , burnBidToken :: Value
  , mintBoughAuctionToken :: Value
  , signatures :: [PubKeyHash]
  , extraIns :: [UTXO]
  , extraOuts :: [UTXO]
  , extraMint :: Value
  }

mustRewardWinner :: BuyNowCtx
mustRewardWinner =
  succeeds
    { winnerReward = mempty
    }

cantMintExtra :: BuyNowCtx
cantMintExtra =
  succeeds
    { extraMint = singleton otherCS "01fc" 3
    }

mustChangeAuction :: BuyNowCtx
mustChangeAuction =
  succeeds
    { auctionOut = succeeds.auctionIn
    }

cantMintMoreBoughtTokens :: BuyNowCtx
cantMintMoreBoughtTokens =
  succeeds
    { extraMint = succeeds.mintBoughAuctionToken
    }

succeeds :: BuyNowCtx
succeeds =
  let ownHash = privateAuctionHash

      bid :: BidEscrow
      bid =
        higherBid
          { status = Bid exampleBidInfo.buyNowPrice 1_500_000
          }

      Just bidderPkh = addressPkh bid.bidder
   in MkBuyNowCtx
        { marketRefIn = marketUtxo
        , auctionIn = auctionUtxo auction LotPresent ownHash
        , winningBidIn = bidEscrowUtxo bid mempty ownHash
        , winnerReward =
            mconcat
              [ address bid.bidder
              , withValue $ normalizeValue $ minAdaVal <> auction.terms.lot
              , withRef ref1
              ]
        , beneficiary1Reward =
            mconcat
              [ address $ fst beneficiary1
              , withValue $ lovelace $ halfBeneficiariesReward SellerCovers
              , withRef ref1
              ]
        , beneficiary2Reward =
            mconcat
              [ address $ fst beneficiary2
              , withValue $ lovelace $ halfBeneficiariesReward SellerCovers
              , withRef ref1
              ]
        , auctionOut = auctionUtxo auction LotBought ownHash
        , marketFees =
            mconcat
              [ address marketAddress
              , withValue $ lovelace fees
              ]
        , burnBidNodeToken = nodeToken (Just bidderPkh) -1
        , burnBidToken = privBidToken -1
        , mintBoughAuctionToken = boughtToken Private 1
        , signatures = [bidderPkh]
        , extraIns = []
        , extraOuts = []
        , extraMint = mempty
        }
