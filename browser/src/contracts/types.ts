import * as Plutus from "../plutus/plutus"
import * as L from "lucid-cardano"

export type Verbosity = "Verbose" | "Conscise"
export type TracingMode = { tag: "DoTracing"; contents: Verbosity } | { tag: "NoTracing" }
export type Config = { tracingMode: TracingMode }

export type AuctionParams = { oref: Plutus.TxOutRef; isPrivate: boolean; config: Config }

export type AuctionTime = { start: Plutus.POSIXTime; close: Plutus.POSIXTime; extension?: TimeExtension }

export type TimeExtension = { window: bigint; length: bigint }

export type AuctionTerms = { lot: Plutus.Value; auctionInfo: AuctionInfo; bidInfo: BidInfo; time: AuctionTime }

export type AuctionInfo = {
  seller: L.AddressDetails
  beneficiaries: Map<L.AddressDetails, bigint>
  sellerToCoverFees: boolean
}

export type BidInfo = { buyNowPrice: bigint; startingPrice: bigint; raiseMinimum: bigint; raisePercentage: bigint }

export type AuctionInstance<Val, MP> = { auctionValidator: Val; nodeValidator: Val; nodeMP: MP; stateMP: MP }

export type MarketInstance<Val, MP> = { marketValidator: Val; marketMP: MP }

export type MarketTerms = { fixedFee: bigint; percentageFee: bigint; address: Plutus.PubKeyHash; minAda: bigint }

// NFT
export type NFTInfo = { tokenName: Plutus.TokenName; meta: L.NFTMetadataDetails }
