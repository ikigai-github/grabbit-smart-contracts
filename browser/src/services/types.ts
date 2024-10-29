import * as Plutus from "../plutus/plutus"
import * as L from "lucid-cardano"
import * as Vulcan from "../contracts/types"
import { lucid } from ".."

export type RawCompiledAuctionInstance = Vulcan.AuctionInstance<
  RawCompiledScript<"SpendingValidator">,
  RawCompiledScript<"MintingPolicy">
>

export type CompiledAuctionInstance = Vulcan.AuctionInstance<
  CompiledScript<"SpendingValidator">,
  CompiledScript<"MintingPolicy">
>

export type RawCompiledMarketInstance = Vulcan.MarketInstance<
  RawCompiledScript<"SpendingValidator">,
  RawCompiledScript<"MintingPolicy">
>

export type CompiledMarketInstance = Vulcan.MarketInstance<
  CompiledScript<"SpendingValidator">,
  CompiledScript<"MintingPolicy">
>

export type ScriptHashType = { MintingPolicy: Plutus.CurrencySymbol; SpendingValidator: Plutus.ValidatorHash }

export type ScriptData<S, H> = { script: S; hash: H }

export const mpHash = (mp: CompiledScript<"MintingPolicy">): L.ScriptHash => mp.hash.unCurrencySymbol

export type CompiledScript<T extends keyof ScriptHashType> = ScriptData<L.Script, ScriptHashType[T]>

export type ScriptType = "PlutusScriptV1" | "PlutusScriptV2"
export type RawCompiledScript<T extends keyof ScriptHashType> = {
  scriptCborHex: string
  hash: ScriptHashType[T]
  version: ScriptType
}

// This may be redundant with <T extends keyof ScriptHashType>, but isn't high priority to figure out now
export type ValidatorType = "SpendingValidator" | "MintingPolicy"

// A typed function may be better. Moving at speed for now.
export const toRawCompiled = (script: L.Script, type: ValidatorType): RawCompiledScript<ValidatorType> =>
  ({
    scriptCborHex: script.script,
    hash: {
      unCurrencySymbol:
        type == "SpendingValidator" ? lucid.utils.validatorToScriptHash(script) : lucid.utils.mintingPolicyToId(script),
    },
    version: "PlutusScriptV2",
  } as RawCompiledScript<typeof type>)

export const toLucidInstance = (instance: RawCompiledAuctionInstance): CompiledAuctionInstance => ({
  nodeValidator: lucidRepr(instance.nodeValidator),
  auctionValidator: lucidRepr(instance.auctionValidator),
  nodeMP: lucidRepr(instance.nodeMP),
  stateMP: lucidRepr(instance.stateMP),
})
function lucidScriptType(t: ScriptType): L.ScriptType {
  switch (t) {
    case "PlutusScriptV1":
      return "PlutusV1"
    case "PlutusScriptV2":
      return "PlutusV2"
  }
  const _exhaustiveCheck: never = t
}

export const scriptCompilerAddress = "http://localhost:8080"

export const lucidRepr = <T extends keyof ScriptHashType>(compiled: RawCompiledScript<T>): CompiledScript<T> => ({
  script: { script: compiled.scriptCborHex, type: lucidScriptType(compiled.version) },
  hash: compiled.hash,
})

export const toLucidMarketInstance = (instance: RawCompiledMarketInstance): CompiledMarketInstance => ({
  marketValidator: lucidRepr(instance.marketValidator),
  marketMP: lucidRepr(instance.marketMP),
})
