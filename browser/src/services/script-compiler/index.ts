import { AuctionParams } from "../../contracts/types"
import {
  RawCompiledAuctionInstance,
  RawCompiledMarketInstance,
  CompiledAuctionInstance,
  CompiledMarketInstance,
  scriptCompilerAddress,
  toLucidMarketInstance,
  toLucidInstance,
  RawCompiledScript,
  toRawCompiled,
} from "../types"
import { applyParamsToScript, Constr, MintingPolicy, Validator } from "lucid-cardano"
import * as contracts from "../../../../compiled/noTrace"

var JSONbig = require("json-bigint")

export function makeInstance(auctionParams: AuctionParams): CompiledAuctionInstance {
  const orefId = new Constr(0, [auctionParams.oref.txOutRefId.getTxId])
  const txOutRef = new Constr(0, [orefId, BigInt(auctionParams.oref.txOutRefIdx)])

  let auctionValidatorScript, stateMPScript: string
  if (auctionParams.isPrivate) {
    auctionValidatorScript = contracts.privateAuctionValidator
    stateMPScript = contracts.privStateTokenScript
  } else {
    auctionValidatorScript = contracts.publicAuctionValidator
    stateMPScript = contracts.pubStateTokenScript
  }

  const auctionVal: Validator = {
    type: "PlutusV2",
    script: auctionValidatorScript,
  }

  const finsetValidator: Validator = {
    type: "PlutusV2",
    script: contracts.finSetValidator,
  }

  const finsetMP: MintingPolicy = {
    type: "PlutusV2",
    script: applyParamsToScript(
      contracts.finSetSepScript,
      /* Includes separator capabilities. Use contracts.finSetScript to exclude separators */ [
        new Constr(0, [txOutRef]),
      ]
    ),
  }

  const rawAuctionVal: RawCompiledScript<"SpendingValidator"> = toRawCompiled(
    auctionVal,
    "SpendingValidator"
  ) as RawCompiledScript<"SpendingValidator">
  const rawFinsetValidator: RawCompiledScript<"SpendingValidator"> = toRawCompiled(
    finsetValidator,
    "SpendingValidator"
  ) as RawCompiledScript<"SpendingValidator">
  const rawFinsetMP: RawCompiledScript<"MintingPolicy"> = toRawCompiled(
    finsetMP,
    "MintingPolicy"
  ) as RawCompiledScript<"MintingPolicy">

  const stateMP: MintingPolicy = {
    type: "PlutusV2",
    script: applyParamsToScript(stateMPScript, [rawFinsetMP.hash.unCurrencySymbol, txOutRef]),
  }

  const rawStateMP: RawCompiledScript<"MintingPolicy"> = toRawCompiled(
    stateMP,
    "MintingPolicy"
  ) as RawCompiledScript<"MintingPolicy">

  const rawCompiledAuction: RawCompiledAuctionInstance = {
    auctionValidator: rawAuctionVal,
    stateMP: rawStateMP,
    nodeValidator: rawFinsetValidator,
    nodeMP: rawFinsetMP,
  }

  window.localStorage.setItem("rawCompiledAuction", JSON.stringify(rawCompiledAuction))
  const instance: CompiledAuctionInstance = toLucidInstance(rawCompiledAuction)

  console.log("Auction Instances successfuly created")
  console.log(instance)
  return instance
}

export async function getInstance(): Promise<CompiledAuctionInstance> {
  const cachedInstance = JSON.parse(window.localStorage.getItem("rawCompiledAuction") ?? "no auction cached")
  console.log(cachedInstance)
  const rawInstance: RawCompiledAuctionInstance = cachedInstance
  const instance: CompiledAuctionInstance = toLucidInstance(rawInstance)
  console.log("Auction Instances successfuly fetched")
  console.log(instance)
  return instance
}

export async function makeMarketInstance(): Promise<CompiledMarketInstance> {
  const address = `${scriptCompilerAddress}/market`
  const params = new Blob([JSONbig.stringify("")], { type: "application/json" })
  const response: Response = await fetch(
    new Request(address, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Request-Headers": "Content-Type",
      },
      body: params,
    })
  )
  try {
    const json: RawCompiledMarketInstance = await response.clone().json()
    const instance: CompiledMarketInstance = toLucidMarketInstance(json)
    console.log("Market Instances successfuly created")
    console.log(instance)
    return instance
  } catch (e) {
    const err: string = await response.clone().text()
    console.log(err)
    throw e
  }
}

export async function getMarketInstance(): Promise<CompiledMarketInstance> {
  const rawInstance: RawCompiledMarketInstance = await require("./market/instances.json")
  const instance: CompiledMarketInstance = toLucidMarketInstance(rawInstance)
  console.log("Market Instances successfuly fetched")
  console.log(instance)
  return instance
}
