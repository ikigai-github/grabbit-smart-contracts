import * as L from "lucid-cardano"
import * as Plutus from "../../plutus/plutus"
import * as Helpers from "../helpers"
import * as Opt from "fp-ts/Option"
import { CompiledAuctionInstance } from "../../services/types"
import toHex from "to-hex"

export const MakeToken = (instance: CompiledAuctionInstance) => ({
  auctionEscrow: auctionEscrowToken(instance),
  bidEscrow: bidEscrowToken(instance),
  boughtEscrow: buyNowEscrowToken(instance),
  registrationEscrow: registrationEscrowToken(instance),
  scriptEscrow: scriptEscrowToken,
  node: {
    setNode: nodeToken(instance),
    corresponding: corrNodeToken(instance),
    head: (amount: bigint) => nodeToken(instance)(Opt.none, amount),
    nonHead: (key: string, amount: bigint) => nodeToken(instance)(Opt.some(key), amount),
  },
})
export const auctionEscrowToken =
  (instance: CompiledAuctionInstance) =>
  (amount: bigint): L.Assets =>
    Helpers.singletonAsset(instance.stateMP.hash, Plutus.tn("AuctionEscrow"), amount)

export const buyNowEscrowToken =
  (instance: CompiledAuctionInstance) =>
  (amount: bigint): L.Assets =>
    Helpers.singletonAsset(instance.stateMP.hash, Plutus.tn("BoughtEscrow"), amount)

export const corrNodeToken =
  (instance: CompiledAuctionInstance) =>
  (amount: bigint): L.Assets =>
    Helpers.singletonAsset(instance.nodeMP.hash, Plutus.tn("FCN"), amount)

export const nodeToken =
  (instance: CompiledAuctionInstance) =>
  (mkey: Opt.Option<string>, amount: bigint): L.Assets =>
    Helpers.singletonAsset(
      instance.nodeMP.hash,
      Opt.match(
        () => Plutus.tn("FSN"),
        (key) => Plutus.tokenName(toHex("FSN") + key)
      )(mkey),
      amount
    )

export const bidEscrowToken =
  (instance: CompiledAuctionInstance) =>
  (amount: bigint): L.Assets =>
    Helpers.singletonAsset(instance.stateMP.hash, Plutus.tn("BidEscrow"), amount)
export const registrationEscrowToken =
  (instance: CompiledAuctionInstance) =>
  (amount: bigint): L.Assets =>
    Helpers.singletonAsset(instance.stateMP.hash, Plutus.tn("RegistrationEscrow"), amount)
export const scriptEscrowToken = (scriptRefCS: Plutus.CurrencySymbol) =>
  Helpers.singletonAsset(scriptRefCS, Plutus.tn("AuctionScript"), 1n)
