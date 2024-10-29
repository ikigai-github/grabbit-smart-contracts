import { Data } from "lucid-cardano"
import { type SchemaToType, addTypeSchema, schemaEncoder, unionEncoder } from "./schema"
import {
  pubKeyHashEncoder,
  addressEncoder,
  refundReasonEncoder,
  booleanEncoder,
  posixTimeEncoder,
  naturalEncoder,
  bidStatusEncoder,
  maybePubKeyHashEncoder,
  currencySymbolEncoder,
  valueEncoder,
  bensEncoder,
  cancelReasonEncoder,
  Nothing,
} from "./plutus-encoder"

export const unit = () => Data.void()

export const MarketSchema = {
  name: "MarketTerms" as const,
  constructor: 0n,
  fields: [
    ["fixedFee", naturalEncoder],
    ["percentageFee", naturalEncoder],
    ["address", addressEncoder],
    ["minAda", naturalEncoder],
  ] as const,
}
addTypeSchema(MarketSchema)
export type MarketTerms = SchemaToType<typeof MarketSchema>

const BidInfoSchema = {
  name: "BidInfo" as const,
  constructor: 0n,
  fields: [
    ["buyNowPrice", naturalEncoder],
    ["startingPrice", naturalEncoder],
    ["raiseMinimum", naturalEncoder],
    ["raisePercentage", naturalEncoder],
  ] as const,
}
addTypeSchema(BidInfoSchema)
export type BidInfo = SchemaToType<typeof BidInfoSchema>

const bidInfoEncoder = schemaEncoder<BidInfo>("BidInfo")

export const AuctionInfoSchema = {
  name: "AuctionInfo" as const,
  constructor: 0n,
  fields: [
    ["seller", addressEncoder],
    ["beneficiaries", bensEncoder],
    ["sellerToCoverFees", booleanEncoder],
  ] as const,
}
addTypeSchema(AuctionInfoSchema)
export type AuctionInfo = SchemaToType<typeof AuctionInfoSchema>

export const auctionInfoEncoder = schemaEncoder<AuctionInfo>("AuctionInfo")

export const TimeExtensionSchema = {
  name: "TimeExtension" as const,
  constructor: 0n,
  fields: [
    ["window", naturalEncoder],
    ["length", naturalEncoder],
  ] as const,
}
addTypeSchema(TimeExtensionSchema)
export type TimeExtension = SchemaToType<typeof TimeExtensionSchema>

export const timeExtensionEncoder = schemaEncoder<TimeExtension>("TimeExtension")

export const JustExtensionSchema = {
  name: "JustExtension" as const,
  constructor: 0n,
  fields: [["extension", timeExtensionEncoder]] as const,
}
addTypeSchema(JustExtensionSchema)
export type JustExtension = SchemaToType<typeof JustExtensionSchema>

export type MaybeExtension = JustExtension | Nothing
export const maybeExtensionEncoder = unionEncoder<MaybeExtension, ["JustExtension", "Nothing"]>([
  "JustExtension",
  "Nothing",
])

export const AuctionTimeSchema = {
  name: "AuctionTime" as const,
  constructor: 0n,
  fields: [
    ["start", posixTimeEncoder],
    ["close", posixTimeEncoder],
    ["maybeExtension", maybeExtensionEncoder],
  ] as const,
}
addTypeSchema(AuctionTimeSchema)
export type AuctionTime = SchemaToType<typeof AuctionTimeSchema>

export const auctionTimeEncoder = schemaEncoder<AuctionTime>("AuctionTime")

export const AuctionTermsSchema = {
  name: "AuctionTerms" as const,
  constructor: 0n,
  fields: [
    ["lot", valueEncoder],
    ["auctionInfo", auctionInfoEncoder],
    ["bidInfo", bidInfoEncoder],
    ["auctionTime", auctionTimeEncoder],
  ] as const,
}
addTypeSchema(AuctionTermsSchema)
export type AuctionTerms = SchemaToType<typeof AuctionTermsSchema>

export const termsEncoder = schemaEncoder<AuctionTerms>("AuctionTerms")

export const AuctionEscrowSchema = {
  name: "AuctionEscrow" as const,
  constructor: 0n,
  fields: [
    ["terms", termsEncoder],
    ["nodeCS", currencySymbolEncoder],
  ] as const,
}
addTypeSchema(AuctionEscrowSchema)
export type AuctionEscrow = SchemaToType<typeof AuctionEscrowSchema>

export const RegistrationEscrowSchema = {
  name: "Registration" as const,
  constructor: 0n,
  fields: [["registrant", addressEncoder]] as const,
}
addTypeSchema(RegistrationEscrowSchema)
export type RegistrationEscrow = SchemaToType<typeof RegistrationEscrowSchema>

export const BidEscrowSchema = {
  name: "BidEscrow" as const,
  constructor: 0n,
  fields: [
    ["status", bidStatusEncoder],
    ["bidder", addressEncoder],
  ] as const,
}
addTypeSchema(BidEscrowSchema)
export type BidEscrow = SchemaToType<typeof BidEscrowSchema>

export const SetNodeSchema = {
  name: "SetNode" as const,
  constructor: 0n,
  fields: [
    ["key", maybePubKeyHashEncoder],
    ["next", maybePubKeyHashEncoder],
  ] as const,
}
addTypeSchema(SetNodeSchema)
export type SetNode = SchemaToType<typeof SetNodeSchema>

export const nodeEncoder = schemaEncoder<SetNode>("SetNode")

const announceSchema = {
  name: "Announce" as const,
  constructor: 0n,
  fields: [] as const,
}
addTypeSchema(announceSchema)
type Announce = SchemaToType<typeof announceSchema>

const enrollSchema = {
  name: "Enroll" as const,
  constructor: 1n,
  fields: [] as const,
}
addTypeSchema(enrollSchema)
type Enroll = SchemaToType<typeof enrollSchema>

const lifecycleSchema = {
  name: "LifeCycle" as const,
  constructor: 2n,
  fields: [] as const,
}
addTypeSchema(lifecycleSchema)
type LifeCycle = SchemaToType<typeof lifecycleSchema>

export type StateMPRedeemer = Announce | Enroll | LifeCycle

const initSchema = {
  name: "Init" as const,
  constructor: 0n,
  fields: [] as const,
}
addTypeSchema(initSchema)
type Init = SchemaToType<typeof initSchema>

const deinitSchema = {
  name: "Deinit" as const,
  constructor: 1n,
  fields: [] as const,
}
addTypeSchema(deinitSchema)
type Deinit = SchemaToType<typeof deinitSchema>

const insertSchema = {
  name: "Insert" as const,
  constructor: 2n,
  fields: [
    ["pk", pubKeyHashEncoder],
    ["node", nodeEncoder],
  ] as const,
}
addTypeSchema(insertSchema)
type Insert = SchemaToType<typeof insertSchema>

const removeSchema = {
  name: "Remove" as const,
  constructor: 3n,
  fields: [
    ["pk", pubKeyHashEncoder],
    ["node", nodeEncoder],
  ] as const,
}
addTypeSchema(removeSchema)
type Remove = SchemaToType<typeof removeSchema>

const removeAndDeninitSchema = {
  name: "RemoveAndDeinit" as const,
  constructor: 4n,
  fields: [["pk", pubKeyHashEncoder]] as const,
}
addTypeSchema(removeAndDeninitSchema)
type RemoveAndDeinit = SchemaToType<typeof removeAndDeninitSchema>

export type SetNodeRedeemer = Init | Deinit | Insert | Remove | RemoveAndDeinit

const cancelActSchema = {
  name: "CancelAct" as const,
  constructor: 0n,
  fields: [
    ["cs", currencySymbolEncoder],
    ["reason", cancelReasonEncoder],
  ] as const,
}
addTypeSchema(cancelActSchema)
type CancelAct = SchemaToType<typeof cancelActSchema>

const bidActSchema = {
  name: "BidAct" as const,
  constructor: 1n,
  fields: [["cs", currencySymbolEncoder]] as const,
}
addTypeSchema(bidActSchema)
type BidAct = SchemaToType<typeof bidActSchema>

const extendActSchema = {
  name: "ExtendAct" as const,
  constructor: 2n,
  fields: [["cs", currencySymbolEncoder]] as const,
}
addTypeSchema(extendActSchema)
type ExtendAct = SchemaToType<typeof extendActSchema>

const buyNowActSchema = {
  name: "BuyNowAct" as const,
  constructor: 4n,
  fields: [["cs", currencySymbolEncoder]] as const,
}
addTypeSchema(buyNowActSchema)
type BuyNowAct = SchemaToType<typeof buyNowActSchema>

const buyNowNoEnrollActSchema = {
  name: "BuyNowNoEnrollAct" as const,
  constructor: 5n,
  fields: [["cs", currencySymbolEncoder], ["receiver", addressEncoder]] as const,
}
addTypeSchema(buyNowNoEnrollActSchema)
type BuyNowNoEnrollAct = SchemaToType<typeof buyNowNoEnrollActSchema>

const refundActSchema = {
  name: "RefundAct" as const,
  constructor: 6n,
  fields: [
    ["cs", currencySymbolEncoder],
    ["reason", refundReasonEncoder],
  ] as const,
}
addTypeSchema(refundActSchema)
type RefundAct = SchemaToType<typeof refundActSchema>

const ResolveActSchema = {
  name: "ResolveAct" as const,
  constructor: 7n,
  fields: [["cs", currencySymbolEncoder]] as const,
}
addTypeSchema(ResolveActSchema)
type ResolveAct = SchemaToType<typeof ResolveActSchema>

const unRegisterSchema = {
  name: "UnRegisterAct" as const,
  constructor: 8n,
  fields: [["cs", currencySymbolEncoder]] as const,
}
addTypeSchema(unRegisterSchema)
type UnRegisterAct = SchemaToType<typeof unRegisterSchema>

const issuePaddleSchema = {
  name: "IssuePaddle" as const,
  constructor: 9n,
  fields: [["cs", currencySymbolEncoder]] as const,
}
addTypeSchema(unRegisterSchema)
type IssuePaddleAct = SchemaToType<typeof issuePaddleSchema>

export type AuctionRedeemer =
  | CancelAct
  | BidAct
  | ExtendAct
  | BuyNowAct
  | BuyNowNoEnrollAct
  | RefundAct
  | ResolveAct
  | UnRegisterAct
  | IssuePaddleAct
