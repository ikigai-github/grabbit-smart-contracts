import * as Arr from "fp-ts/lib/Array"
import { map } from "fp-ts/lib/Map"
import * as L from "lucid-cardano"
import * as Helpers from "../contracts/helpers"
import toHex from "to-hex"

export const HASH_LENGTH = 56

export type Script = { script: L.Script; type: L.ScriptHash }
export type ADA = bigint
export type TokenName = { unTokenName: string }
export type POSIXTime = { getPOSIXTime: number }
export type ValidatorHash = string
export type CurrencySymbol = { unCurrencySymbol: string }
export type PubKeyHash = { getPubKeyHash: string }
export type TxOutRef = { txOutRefId: { getTxId: string }; txOutRefIdx: number }
/* The regular Map used for safiety (keys uniqeness, ordering),
convert to AssocMap to interact with haskell */
export type Value = Map<CurrencySymbol, Map<TokenName, bigint>>
export type PlutusValue = { getValue: AssocMap<CurrencySymbol, AssocMap<TokenName, bigint>> }

// this is how PlutusTx.AssocMap represented in JSON
export type AssocMap<K, V> = { unMap: [K, V][] }

export const mapToArray = <K, V>(map: Map<K, V>): [K, V][] => [...map.entries()]
export const mapToAssocMap = <K, V>(map: Map<K, V>): AssocMap<K, V> => ({ unMap: [...map.entries()] })

export const toPlutusValue = (x: Value): PlutusValue => ({
  getValue: { unMap: mapToArray(x).map(([cs, tokens]) => [cs, mapToAssocMap<TokenName, bigint>(tokens)]) },
})

// Constructors, deconstructors

export const csOf = ({ unCurrencySymbol: cs }: CurrencySymbol): string => cs
export const tnOf = ({ unTokenName: tn }: TokenName): string => tn
export const tokenName = (str: string): TokenName => ({ unTokenName: str })
export const tn = (str: string): TokenName => ({ unTokenName: toHex(str) })
// Expects valid cs length
export const cs = (cs: string): CurrencySymbol => ({ unCurrencySymbol: assertValidCSHashLength(cs) })
// Expects valid pkh length
export const pkh = (pkh: string): PubKeyHash => ({ getPubKeyHash: assertValidHashLength(pkh) })

export const timeOf = (t: number): POSIXTime => ({ getPOSIXTime: t })

export function assertValidCSHashLength(hash: string): string | never {
  if (hash.length == 0 || hash.length == HASH_LENGTH) {
    return hash
  } else {
    throw Error(`Unexpected hash length, expected ${HASH_LENGTH} or 0 symbols, got ${hash.length}`)
  }
}

export function assertValidHashLength(hash: string): string | never {
  if (hash.length == HASH_LENGTH) {
    return hash
  } else {
    throw Error(`Unexpected hash length, expected ${HASH_LENGTH} symbols, got ${hash.length}`)
  }
}

// Conversion between representations

export const valueToAssets = (val: Value): L.Assets =>
  Object.fromEntries(
    Arr.chain<[CurrencySymbol, Map<TokenName, bigint>], [string, bigint]>(([cs, tokens]) =>
      Object.entries(Helpers.asset(cs, map(BigInt)(tokens)))
    )(mapToArray(val))
  )

export const schemaToAssets = (val: Map<string, Map<string, bigint>>): L.Assets =>
  Object.fromEntries(
    Arr.chain<[string, Map<string, bigint>], [string, bigint]>(([cs, tokens]) =>
      Object.entries(Helpers.schemaToasset(cs, map(BigInt)(tokens)))
    )(mapToArray(val))
  )

export const hasCS = (cs: CurrencySymbol, assets: L.Assets): boolean => {
  return Object.entries(assets).some(([k, _]) => {
    return k.slice(0, 56) === cs.unCurrencySymbol
  })
}
