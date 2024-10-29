import * as L from "lucid-cardano"
import {
  Encoder,
  SchemaToType,
  addTypeSchema,
  bigintEncoder,
  validMapEncoder,
  schemaEncoder,
  unionEncoder
} from "./schema"

const falseSchema = {
  name: "False" as const,
  constructor: 0n,
  fields: [] as const,
}
addTypeSchema(falseSchema)
export type False = SchemaToType<typeof falseSchema>
export const False = { kind: "False" }

const trueSchema = {
  name: "True" as const,
  constructor: 1n,
  fields: [] as const,
}
addTypeSchema(trueSchema)
export type True = SchemaToType<typeof trueSchema>
export const True = { kind: "True" }

export type Boolean = False | True
export const booleanEncoder = unionEncoder<Boolean, ["False", "True"]>(["False", "True"])

export type NonZero = bigint & { readonly NonZero: unique symbol }
const isNonZero = (n: bigint): n is NonZero => n !== 0n
const nonZero = (input: bigint): NonZero => {
  if (!isNonZero(input)) throw new Error("Unexpected zero value")
  return input
}
export const nonZeroEncoder: Encoder<bigint, NonZero> = { name: "bigint", validator: nonZero, proxy: null }

export type Natural = bigint & { readonly Natural: unique symbol }
const isNatural = (n: bigint): n is Natural => n >= 0n
const natural = (input: bigint): Natural => {
  if (!isNatural(input)) throw new Error("Unexpected non-positive value")
  return input
}
export const naturalEncoder: Encoder<bigint, Natural> = { name: "bigint", validator: natural, proxy: null }

export type Positive = bigint & { readonly Positive: unique symbol }
const isPositive = (n: bigint): n is Positive => n > 0n
const positive = (input: bigint): Positive => {
  if (!isPositive(input)) throw new Error("Unexpected non-positive value")
  return input
}
export const positiveEncoder: Encoder<bigint, Positive> = { name: "bigint", validator: positive, proxy: null }

type BSOfLength<N> = string & { readonly BSOfLength: unique symbol }
const isBSOfLength = <N extends number>(n: N, s: string): s is BSOfLength<N> =>
  new RegExp(`^[0-9A-Fa-f]{${n * 2}}$`).test(s)
const bsOfLength =
  <N extends number>(n: N) =>
  (input: string): BSOfLength<N> => {
    if (!isBSOfLength(n, input)) throw new Error(`Invalid ${n}-byte bytestring`)
    return input
  }
export const bsOfLengthEncoder = <N extends number>(n: N): Encoder<string, BSOfLength<N>> => ({
  name: "string",
  validator: bsOfLength(n),
  proxy: null,
})

type BS28 = BSOfLength<28>
export const bs28Encoder: Encoder<string, BS28> = bsOfLengthEncoder(28)
type BS32 = BSOfLength<32>
export const bs32Encoder: Encoder<string, BS32> = bsOfLengthEncoder(32)

export type PubKeyHash = BS28
export const pubKeyHashEncoder = bs28Encoder
export type ScriptHash = BS28
export const scriptHashEncoder = bs28Encoder
export type ValidatorHash = BS28
export const validatorHashEncoder = bs28Encoder
export type MintingPolicyHash = BS28
export const mintingPolicyHashEncoder = bs28Encoder
export type DatumHash = BS32
export const datumHashEncoder = bs32Encoder
export type RedeemerHash = BS32
export const redeemerHashEncoder = bs32Encoder

export type CurrencySymbol = string & { readonly CurrencySymbol: unique symbol }
export const currencySymbol = (input: string): CurrencySymbol => {
  const isCurrencySymbol = (s: string): s is CurrencySymbol => s === "" || isBSOfLength(28, s)
  if (!isCurrencySymbol(input)) throw new Error("Invalid currency symbol")
  return input
}
export const currencySymbolEncoder: Encoder<string, CurrencySymbol> = {
  name: "string",
  validator: currencySymbol,
  proxy: null,
}

export type TokenName = string & { readonly TokenName: unique symbol }
export const tokenName = (input: string): TokenName => {
  const isTokenName = (s: string): s is TokenName => /^([0-9A-Fa-f]{2}){0,32}$/.test(s)
  if (!isTokenName(input)) throw new Error("Invalid token name")
  return input
}
export const tokenNameEncoder: Encoder<string, TokenName> = { name: "string", validator: tokenName, proxy: null }

export type Value = Map<CurrencySymbol, Map<TokenName, bigint>>
export const valueEncoder = validMapEncoder<string, CurrencySymbol, Map<string, bigint>, Map<TokenName, bigint>>(
  currencySymbolEncoder,
  validMapEncoder<string, TokenName, bigint, bigint>(tokenNameEncoder, bigintEncoder)
)

export const positiveValueEncoder = validMapEncoder<
  string,
  CurrencySymbol,
  Map<string, bigint>,
  Map<TokenName, Positive>
>(currencySymbolEncoder, validMapEncoder<string, TokenName, bigint, Positive>(tokenNameEncoder, positiveEncoder))

export const nonZeroValueEncoder = validMapEncoder<
  string,
  CurrencySymbol,
  Map<string, bigint>,
  Map<TokenName, NonZero>
>(currencySymbolEncoder, validMapEncoder<string, TokenName, bigint, NonZero>(tokenNameEncoder, nonZeroEncoder))

export const posixTimeEncoder = naturalEncoder

export const assetsToValue = (assets: L.Assets): Value => {
  const value: Value = new Map()
  Object.entries(assets).forEach(([k, v]) => {
    const cs = currencySymbol(k.slice(0, 56))
    const tn = tokenName(k.slice(56))
    if (!value.has(cs)) value.set(cs, new Map())
    value.get(cs)!.set(tn, v)
  })
  return value
}

export const hasCS = (val: Value, cs: CurrencySymbol): boolean => {
  return val.has(cs)
}

export const valueToAssets = (value: Value): L.Assets => {
  const assets: L.Assets = {}
  value.forEach((t, cs) => {
    t.forEach((v, tn) => {
      assets[cs + tn] = v
    })
  })
  return assets
}
const nothingSchema = {
  name: "Nothing" as const,
  constructor: 1n,
  fields: [] as const,
}
addTypeSchema(nothingSchema)
export type Nothing = SchemaToType<typeof nothingSchema>
export const Nothing = { kind: "Nothing" }

export const isNothing = (v: any) => v === "Nothing"

const justPubKeyHashSchema = {
  name: "JustPKH" as const,
  constructor: 0n,
  fields: [["pubKey", pubKeyHashEncoder]] as const,
}
addTypeSchema(justPubKeyHashSchema)
export type JustPKH = SchemaToType<typeof justPubKeyHashSchema>
export const JustPKH = { kind: "JustPKH" }

export type MaybePubKeyHash = JustPKH | Nothing
export const maybePubKeyHashEncoder = unionEncoder<MaybePubKeyHash, ["JustPKH", "Nothing"]>(["JustPKH", "Nothing"])

const noBidSchema = {
  name: "NoBid" as const,
  constructor: 0n,
  fields: [] as const,
}
addTypeSchema(noBidSchema)
export type NoBid = SchemaToType<typeof noBidSchema>
export const NoBid = { kind: "NoBid" }

export const isNoBid = (v: any) => v === "NoBid"

const bidSchema = {
  name: "Bid" as const,
  constructor: 1n,
  fields: [
    ["bid", naturalEncoder],
    ["time", posixTimeEncoder],
  ] as const,
}
addTypeSchema(bidSchema)
export type Bid = SchemaToType<typeof bidSchema>
export const Bid = { kind: "Bid" }

export type BidStatus = NoBid | Bid
export const bidStatusEncoder = unionEncoder<BidStatus, ["NoBid", "Bid"]>(["NoBid", "Bid"])

const lostBidSchema = {
  name: "LostBid" as const,
  constructor: 0n,
  fields: [] as const,
}
addTypeSchema(lostBidSchema)
type LostBid = SchemaToType<typeof lostBidSchema>

const boughtLotSchema = {
  name: "BoughtLot" as const,
  constructor: 1n,
  fields: [] as const,
}
addTypeSchema(boughtLotSchema)
type BoughtLot = SchemaToType<typeof boughtLotSchema>

export type RefundReason = LostBid | BoughtLot

export const refundReasonEncoder = unionEncoder<RefundReason, ["LostBid", "BoughtLot"]>(["LostBid", "BoughtLot"])

const failedSchema = {
  name: "Failed" as const,
  constructor: 0n,
  fields: [] as const,
}
addTypeSchema(failedSchema)
type Failed = SchemaToType<typeof failedSchema>

const boughtSchema = {
  name: "Bought" as const,
  constructor: 1n,
  fields: [] as const,
}
addTypeSchema(boughtSchema)
type Bought = SchemaToType<typeof boughtSchema>

export type CancelReason = Failed | Bought

export const cancelReasonEncoder = unionEncoder<CancelReason, ["Failed", "Bought"]>(["Failed", "Bought"])

const pubKeyCredentialSchema = {
  name: "PubKeyCredential" as const,
  constructor: 0n,
  fields: [["hash", bs28Encoder]] as const,
}
addTypeSchema(pubKeyCredentialSchema)
export type PubKeyCredential = SchemaToType<typeof pubKeyCredentialSchema>

const scriptCredentialSchema = {
  name: "ScriptCredential" as const,
  constructor: 1n,
  fields: [["hash", bs28Encoder]] as const,
}
addTypeSchema(scriptCredentialSchema)
export type ScriptCredential = SchemaToType<typeof scriptCredentialSchema>

export type Credential = PubKeyCredential | ScriptCredential
export const credentialEncoder = unionEncoder<Credential, ["PubKeyCredential", "ScriptCredential"]>([
  "PubKeyCredential",
  "ScriptCredential",
])

const stakingHashSchema = {
  name: "StakingHash" as const,
  constructor: 0n,
  fields: [["credential", credentialEncoder]] as const,
}
addTypeSchema(stakingHashSchema)
export type StakingHash = SchemaToType<typeof stakingHashSchema>

const stakingPtrSchema = {
  name: "StakingPtr" as const,
  constructor: 1n,
  fields: [
    ["slotIndex", bigintEncoder],
    ["txIndex", bigintEncoder],
    ["dcertIndex", bigintEncoder],
  ] as const,
}
addTypeSchema(stakingPtrSchema)
export type StakingPtr = SchemaToType<typeof stakingPtrSchema>

export type StakingCredential = StakingHash | StakingPtr
export const stakingCredentialEncoder = unionEncoder<StakingCredential, ["StakingHash", "StakingPtr"]>([
  "StakingHash",
  "StakingPtr",
])

const justScriptHashSchema = {
  name: "JustScriptHash" as const,
  constructor: 0n,
  fields: [["hash", scriptHashEncoder]] as const,
}
addTypeSchema(justScriptHashSchema)
export type JustScriptHash = SchemaToType<typeof justScriptHashSchema>

export type MaybeScriptHash = JustScriptHash | Nothing
export const maybeScriptHashEncoder = unionEncoder<MaybeScriptHash, ["JustScriptHash", "Nothing"]>([
  "JustScriptHash",
  "Nothing",
])

const txIdSchema = {
  name: "TxId" as const,
  constructor: 0n,
  fields: [["id", bs32Encoder]] as const,
}
addTypeSchema(txIdSchema)
export type TxId = SchemaToType<typeof txIdSchema>
export const txIdEncoder = schemaEncoder<TxId>("TxId")
const txOutRefSchema = {
  name: "TxOutRef" as const,
  constructor: 0n,
  fields: [
    ["txId", txIdEncoder],
    ["txIdx", naturalEncoder],
  ] as const,
}
addTypeSchema(txOutRefSchema)
export type TxOutRef = SchemaToType<typeof txOutRefSchema>
export const txOutRefEncoder = schemaEncoder<TxOutRef>("TxOutRef")

const justStakingCredentialSchema = {
  name: "JustStakingCredential" as const,
  constructor: 0n,
  fields: [["stakingCredential", stakingCredentialEncoder]] as const,
}
addTypeSchema(justStakingCredentialSchema)
type JustStakingCredential = SchemaToType<typeof justStakingCredentialSchema>

export type MaybeStakingCredential = JustStakingCredential | Nothing
export const maybeStakingCredentialEncoder = unionEncoder<MaybeStakingCredential, ["JustStakingCredential", "Nothing"]>(
  ["JustStakingCredential", "Nothing"]
)

const addressSchema = {
  name: "Address" as const,
  constructor: 0n,
  fields: [
    ["paymentCredential", credentialEncoder],
    ["stakingCredential", maybeStakingCredentialEncoder],
  ] as const,
}
addTypeSchema(addressSchema)
export type Address = SchemaToType<typeof addressSchema>
export const addressEncoder = schemaEncoder<Address>("Address")

export const bensEncoder = validMapEncoder<Address, Address, bigint, Positive>(addressEncoder, positiveEncoder)
