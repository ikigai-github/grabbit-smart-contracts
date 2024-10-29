import * as L from "lucid-cardano"
import * as Plutus from "../plutus/plutus"
import type * as Vulcan from "./types"
import * as Schema from "../data/schema"
import * as Encoder from "../data/plutus-encoder"
import * as Datums from "../data/datums"
import { Lucid, Utils, UTxO } from "lucid-cardano"
import type { CompiledMarketInstance } from "../services/types"
import { pipe } from "fp-ts/lib/function"
import { makeInstance } from "../services/script-compiler"
import { MakeToken } from "./helpers/contract"

// General purpose

// Utils

export const geq = (a: L.Assets, b: L.Assets) =>
  Array(...Object.entries(b))
    .sort()
    .every(([asset, amountB]: [string, bigint]) =>
      maybe(
        () => false,
        (amountA: bigint) => amountA >= amountB
      )(a[asset])
    )

export const hasTxOutRef =
  (oref: Plutus.TxOutRef) =>
  (utxo: L.UTxO): boolean =>
    utxo.txHash === oref.txOutRefId.getTxId && utxo.outputIndex === oref.txOutRefIdx

export const utxoOutRef = (x: L.UTxO): Plutus.TxOutRef => ({
  txOutRefId: { getTxId: x.txHash },
  txOutRefIdx: x.outputIndex,
})

export const mapMapKVs =
  <K, V, K1, V1>(f: (k: K, v: V) => [K1, V1]) =>
  (map: Map<K, V>): Map<K1, V1> => {
    const result = new Map()
    map.forEach((v, k) => pipe(f(k, v), ([k1, v1]) => result.set(k1, v1)))
    return result
  }

export const removeAsset =
  (asset: string) =>
  (assets: L.Assets): L.Assets =>
    Object.fromEntries(
      Object.keys(assets)
        .filter((asset0) => asset0 != asset)
        .map((asset0) => [asset0, assets[asset0]!])
    )

export const assetName = (cs: Plutus.CurrencySymbol, tn: Plutus.TokenName): string =>
  concat(Plutus.csOf(cs), Plutus.tnOf(tn))

export const lovelaceValueOf = (assets: L.Assets): bigint => {
  const lovelace = Object.entries(assets).find(([key, _]) => key == "lovelace")!
  return lovelace[1]
}

export function asset(cs: Plutus.CurrencySymbol, tokens: Map<Plutus.TokenName, bigint>): L.Assets {
  if (Plutus.csOf(cs) == "") {
    console.log("Found ada!")
    return {
      lovelace: Plutus.mapToArray(tokens).reduce((acc, [_, amt]) => acc + amt, 0n),
    }
  } else {
    return Object.fromEntries(Plutus.mapToArray(tokens).map(([tn, n]) => [concat(Plutus.csOf(cs), Plutus.tnOf(tn)), n]))
  }
}

export function schemaToasset(cs: string, tokens: Map<string, bigint>): L.Assets {
  if (cs == "") {
    console.log("Found ada!")
    return {
      lovelace: Plutus.mapToArray(tokens).reduce((acc, [_, amt]) => acc + amt, 0n),
    }
  } else {
    return Object.fromEntries(Plutus.mapToArray(tokens).map(([tn, n]) => [concat(cs, tn), n]))
  }
}

export const singletonAsset = (cs: Plutus.CurrencySymbol, tokenName: Plutus.TokenName, amount: bigint): L.Assets =>
  Plutus.valueToAssets(singleton(cs, tokenName, amount))

export const adaAsset = (lovelace: bigint): L.Assets => ({ lovelace })

export function singleton(cs: Plutus.CurrencySymbol, tokenName: Plutus.TokenName, amount: bigint): Plutus.Value {
  return new Map([[cs, new Map([[tokenName, amount]])]])
}

// Concat strings without implicit cast to string
export const concat = (...strs: string[]): string => strs.reduce((acc, s) => acc + s, "")

export const print = (x: any) => console.log(x)
export const maybe =
  <A, B>(ifZero: () => B, f: (_: A) => B) =>
  (x: A | undefined) =>
    x ? f(x) : ifZero()

export const assert = (b: boolean, err?: string, consolelog?: boolean): void => {
  if (!b) {
    if (consolelog) console.log(err)
    throw new Error(err)
  }
}

// Contract helpers

export const getLot = (lucid: L.Lucid, auction: Vulcan.AuctionTerms): Promise<L.UTxO> =>
  getUtxo(lucid, Plutus.valueToAssets(auction.lot), auction.auctionInfo.seller.address.bech32)

// UTxOs

export async function getUtxo(lucid: L.Lucid, value: L.Assets, address: L.Address): Promise<L.UTxO> {
  const utxos = await lucid.utxosAt(address)
  const result = findUtxo(value, utxos)
  if (result == undefined) {
    throw Error("getUtxo: No UTxOs found")
  }
  return result
}

export async function getNodeUtxos(
  lucid: L.Lucid,
  cs: Plutus.CurrencySymbol,
  address: L.Address
): Promise<[L.UTxO, Datums.SetNode][]> {
  const utxos = await lucid.utxosAt(address)
  const result = findNodeUtxos(cs, utxos)
  if (result == undefined) {
    throw Error("getUtxo: No UTxOs found")
  }
  return result.map(tryParseNodes)
}

export async function getUtxos(lucid: L.Lucid, value: L.Assets, address: L.Address): Promise<L.UTxO[]> {
  const utxos = await lucid.utxosAt(address)
  const result = findUtxos(value, utxos)
  if (result == undefined) {
    throw Error("getUtxo: No UTxOs found")
  }
  return result
}

export async function getBidUtxosFromParams(lucid: L.Lucid, params: Vulcan.AuctionParams) {
  const instance = makeInstance(params)
  const address = lucid.utils.validatorToAddress(instance.auctionValidator.script)

  return getBidUtxos(lucid, instance.stateMP.hash, address)
}

export async function getBidUtxos(
  lucid: L.Lucid,
  cs: Plutus.CurrencySymbol,
  address: L.Address
): Promise<[L.UTxO, Datums.BidEscrow][]> {
  const value = singletonAsset(cs, Plutus.tn("BidEscrow"), 1n)
  const utxos = await lucid.utxosAt(address)
  const result = findUtxos(value, utxos)
  if (result == undefined) {
    throw Error("getUtxo: No UTxOs found")
  }
  return result.map(tryParseBidEscrow)
}

export async function getAuctionUtxosFromParams(lucid: L.Lucid, params: Vulcan.AuctionParams) {
  const instance = makeInstance(params)
  const address = lucid.utils.validatorToAddress(instance.auctionValidator.script)

  return getAuctionUtxos(lucid, address)
}

export async function getAuctionUtxos(lucid: L.Lucid, address: L.Address): Promise<L.UTxO[]> {
  const utxos = await lucid.utxosAt(address)
  const result = findUtxosByTokenName(Plutus.tn("AuctionEscrow"), utxos)
  if (result == undefined) {
    throw Error("getUtxo: No UTxOs found")
  }
  return result
}

export async function getMarketUTxO(lucid: Lucid, instance: CompiledMarketInstance): Promise<L.UTxO> {
  const utils = new Utils(lucid)
  const marketToken = singletonAsset(instance.marketMP.hash, Plutus.tn("MarketEscrow"), 1n)
  const marketVal = instance.marketValidator.script
  const marketUtxo = await getUtxo(lucid, marketToken, utils.validatorToAddress(marketVal))
  return marketUtxo
}

export async function auctionIsResolved(lucid: L.Lucid, params: Vulcan.AuctionParams): Promise<boolean> {
  const instance = makeInstance(params)
  const address = lucid.utils.validatorToAddress(instance.auctionValidator.script)
  const utxos = await lucid.utxosAt(address)

  const tokenFactory = MakeToken(instance)
  const result = findUtxos(tokenFactory.auctionEscrow(1n), utxos)

  return result == undefined
}

export const findNodeUtxos = (cs: Plutus.CurrencySymbol, utxos: L.UTxO[]): L.UTxO[] =>
  utxos.filter((utxo: L.UTxO) => Plutus.hasCS(cs, utxo.assets))!

export const findUtxo = (value: L.Assets, utxos: L.UTxO[]): L.UTxO | undefined =>
  utxos.find((utxo: L.UTxO) => geq(utxo.assets, value))

export const findUtxos = (value: L.Assets, utxos: L.UTxO[]): L.UTxO[] | undefined =>
  utxos.filter((utxo: L.UTxO) => geq(utxo.assets, value))

export const findUtxosByTokenName = (name: Plutus.TokenName, utxos: L.UTxO[]): L.UTxO[] =>
  utxos.filter((utxo: L.UTxO) => {
    if (utxo.assets == undefined || utxo.assets[0] == undefined) return false
    return utxo.assets[0].toString().endsWith(name.unTokenName)
  })

export const findUtxosByCurrencySymbol = (cs: Plutus.CurrencySymbol, utxos: L.UTxO[]): L.UTxO[] =>
  utxos.filter((utxo: L.UTxO) => {
    if (utxo.assets == undefined || utxo.assets[0] == undefined) return false
    return utxo.assets[0].toString().startsWith(cs.unCurrencySymbol)
  })

export const findHighestBid = (utxos: L.UTxO[]): bigint => {
  const winner = findHighestBidder(utxos)

  return getBidFromUtxo(winner)
}

export const findHighestBidder = (utxos: L.UTxO[]): L.UTxO => {
  const compareFn = (a: UTxO, b: UTxO) => Number(getBidFromUtxo(a) - getBidFromUtxo(b)) // not very efficient, start here if we need to optimize
  const winner = utxos.sort(compareFn).pop()

  if (winner == undefined) throw new Error("No bids found")
  return winner
}

export const sortOn = <T>(on: (_: T) => bigint, xs: T[]): T[] => xs.sort((a, b) => Number(on(a) - on(b)))

export const sortBids = <T>(bids: [T, Datums.BidEscrow][]): [T, Datums.BidEscrow][] =>
  sortOn(([, bid]) => getBid(bid), bids)

export const sortBidsByTime = <T>(bids: [T, Datums.BidEscrow][]): [T, Datums.BidEscrow][] =>
  sortOn(([, bid]) => getTime(bid), bids)

export function getBidFromUtxo(utxo: L.UTxO) {
  const bidEscrow = bidDatum(utxo)
  return getBid(bidEscrow)
}

// Datums

export function bidDatum(utxo: L.UTxO): Datums.BidEscrow {
  if (typeof utxo.datum === undefined) {
    throw new Error("No inline datum attached")
  }
  const data = L.Data.from(utxo.datum!)
  return Schema.fromData(Datums.BidEscrowSchema, Schema.plutusDataToData(data))
}
export function tryParseBidEscrow(utxo: L.UTxO): [L.UTxO, Datums.BidEscrow] {
  if (typeof utxo.datum === undefined) {
    throw new Error("No inline datum attached")
  }
  const data = L.Data.from(utxo.datum!)
  return [utxo, Schema.fromData(Datums.BidEscrowSchema, Schema.plutusDataToData(data))]
}
export function tryParseNodes(utxo: L.UTxO): [L.UTxO, Datums.SetNode] {
  if (typeof utxo.datum === undefined) {
    throw new Error("No inline datum attached")
  }
  const data = L.Data.from(utxo.datum!)
  return [utxo, Schema.fromData(Datums.SetNodeSchema, Schema.plutusDataToData(data))]
}
export function regDatum(utxo: L.UTxO): Datums.RegistrationEscrow {
  if (typeof utxo.datum === undefined) {
    throw new Error("No inline datum attached")
  }
  const data = L.Data.from(utxo.datum!)
  return Schema.fromData(Datums.RegistrationEscrowSchema, Schema.plutusDataToData(data))
}

export function nodeDatum(utxo: L.UTxO): Datums.SetNode {
  if (typeof utxo.datum === undefined) {
    throw new Error("No inline datum attached")
  }
  const data = L.Data.from(utxo.datum!)
  return Schema.fromData(Datums.SetNodeSchema, Schema.plutusDataToData(data))
}

export function getMarketDatum(utxo: L.UTxO): Datums.MarketTerms {
  if (typeof utxo.datum === undefined) {
    throw new Error("No inline datum attached")
  }
  const data = L.Data.from(utxo.datum!)
  return Schema.fromData(Datums.MarketSchema, Schema.plutusDataToData(data))
}

export function getAuctionDatum(utxo: L.UTxO): Datums.AuctionEscrow {
  assert(!(typeof utxo.datum === undefined), "No inline datum attached")
  const data = L.Data.from(utxo.datum!)
  return Schema.fromData(Datums.AuctionEscrowSchema, Schema.plutusDataToData(data))
}

// Auction

export const auctionToken = (cs: Plutus.CurrencySymbol, amount: bigint): L.Assets =>
  singletonAsset(cs, Plutus.tn("AuctionEscrow"), amount)

export const boughtToken = (cs: Plutus.CurrencySymbol, amount: bigint): L.Assets =>
  singletonAsset(cs, Plutus.tn("BoughtEscrow"), amount)

export const hasBoughtToken = (utxo: L.UTxO, cs: Plutus.CurrencySymbol): boolean =>
  geq(utxo.assets, boughtToken(cs, 1n))

// Nodes

export const nodeToken = (cs: Plutus.CurrencySymbol, amount: bigint): L.Assets =>
  singletonAsset(cs, Plutus.tn("FSN"), amount)

export const findCoveringNode = (pk: string, utxos: L.UTxO[]) => utxos.find((utxo: L.UTxO) => nodeCovers(utxo, pk))

function nodeCovers(utxo: L.UTxO, pk: string) {
  const node = nodeDatum(utxo)
  return covers(node, pk)
}

export const findNodeWithKey = (pk: string, utxos: L.UTxO[]) => utxos.find((utxo: L.UTxO) => nodeHasKey(utxo, pk))!

function nodeHasKey(utxo: L.UTxO, pk: string) {
  const node = nodeDatum(utxo)
  return !Encoder.isNothing(node.key.kind) && (node.key as Encoder.JustPKH).pubKey === pk
}

export const findNodeWithNext = (pk: string, utxos: L.UTxO[]) => utxos.find((utxo: L.UTxO) => nodeHasNext(utxo, pk))!

function nodeHasNext(utxo: L.UTxO, pk: string) {
  const node = nodeDatum(utxo)
  return !Encoder.isNothing(node.next.kind) && (node.next as Encoder.JustPKH).pubKey === pk
}

const covers = (node: Datums.SetNode, pk: string): boolean => {
  return (
    (Encoder.isNothing(node.key.kind) || (node.key as Encoder.JustPKH).pubKey < pk) &&
    (Encoder.isNothing(node.next.kind) || (node.next as Encoder.JustPKH).pubKey > pk)
  )
}

export const getPK = (mpk: Encoder.MaybePubKeyHash) => {
  switch (mpk.kind) {
    case "Nothing":
      return ""
    case "JustPKH":
      return (mpk as Encoder.JustPKH).pubKey
  }
}

// Registration Escrows

export const findRegUtxo = (pk: string, utxos: L.UTxO[]) => utxos.find((utxo: L.UTxO) => hasRegPK(utxo, pk))

const hasRegPK = (utxo: L.UTxO, pk: string): boolean => pk === regDatum(utxo).registrant.paymentCredential.hash!

// Bid Escrows

export const getBid = (bidDatum: Datums.BidEscrow): bigint => {
  const status = bidDatum.status
  return findBid(status)
}

export const getTime = (bidDatum: Datums.BidEscrow): bigint => findTime(bidDatum.status)

function findBid(status: Encoder.BidStatus): bigint {
  switch (status.kind) {
    case "NoBid":
      return 0n
    case "Bid":
      return (status as Encoder.Bid).bid
  }
}

function findTime(status: Encoder.BidStatus): bigint {
  switch (status.kind) {
    case "NoBid":
      return 0n
    case "Bid":
      return (status as Encoder.Bid).time
  }
}

export const findBidUtxo = (pk: string, utxos: L.UTxO[]) => utxos.find((utxo: L.UTxO) => hasbidder(utxo, pk))

export const findHigherBidUtxo = (bid: bigint, utxos: L.UTxO[]) => utxos.find((utxo: L.UTxO) => hasHigherBid(utxo, bid))

const hasbidder = (utxo: L.UTxO, pk: string): boolean => pk === bidDatum(utxo).bidder.paymentCredential.hash!

const hasHigherBid = (utxo: L.UTxO, bid: bigint): boolean => bid < getBid(bidDatum(utxo))

// may throw
export const ada = (amount: bigint): Plutus.Value => new Map([[Plutus.cs(""), new Map([[Plutus.tn(""), amount]])]])

// Time
export const beforeTime = (time: number) => time - 20_000;
export const afterTime = (time: number) => time + 20_000;
export const addTime = 120_000; // two minutes by default
export const addBidTime = 125_000;

export const tolovelace = (amount: bigint) => amount * 1_000_000n;
export const fromlovelace = (amount: bigint) => amount / 1_000_000n;

export function getExtension(
	extension: Datums.MaybeExtension
): Datums.JustExtension {
	switch (extension.kind) {
		case 'JustExtension':
			return extension as Datums.JustExtension;
		case 'Nothing':
			throw new Error('Auction is not extendable');
	}
}
