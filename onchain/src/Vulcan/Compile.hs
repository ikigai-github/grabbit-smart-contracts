module Vulcan.Compile (auctionInstance) where

import Plutarch.Api.V2 (mintingPolicySymbol)
import Ply qualified

import Vulcan.Common.Types.Instances (
  AuctionInstance,
  AuctionInstance' (
    MkAuctionInstance,
    auctionValidator,
    nodeMP,
    nodeValidator,
    stateMP
  ),
  AuctionParams (oref),
  AuctionScripts (
    auctionScript,
    nodeMPScript,
    nodeScript,
    stateMPScript
  ),
  FinSetScript (..),
 )

auctionInstance :: AuctionScripts -> AuctionParams -> AuctionInstance
auctionInstance scripts params =
  MkAuctionInstance
    { nodeValidator = Ply.toValidator $ scripts.nodeScript
    , auctionValidator = Ply.toValidator $ scripts.auctionScript
    , nodeMP = nodeMP
    , stateMP = Ply.toMintingPolicy $ scripts.stateMPScript Ply.# nodeCS Ply.# params.oref
    }
  where
    nodeScript = case scripts.nodeMPScript of
      StandardFinSet script -> script Ply.# params.oref
    nodeMP = Ply.toMintingPolicy nodeScript
    nodeCS = mintingPolicySymbol nodeMP
