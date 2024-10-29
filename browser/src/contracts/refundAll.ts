import * as L from "lucid-cardano"
import * as Helpers from "../contracts/helpers"
import * as Datums from "../data/datums"
import { refund } from "../contracts/auction/common"
import { Lucid, Utils } from "lucid-cardano"
import { CompiledAuctionInstance, CompiledMarketInstance } from "../services/types"

const sign = "22c5cb315ef74f0d062a63d13c0aacfddab642c5e17e69a75a8c0970b207832a"
const addr = "addr_test1vppksvg44qqjtkgt6q8e4n3kmxcf2fg7vshtryr8armrezczlvlc0"

export async function refundNodes(
  instance: CompiledAuctionInstance,
  marketInstance: CompiledMarketInstance,
  lucid: Lucid,
  utils: Utils,
  winner?: string
) {
  const nodeAddress = utils.validatorToAddress(instance.nodeValidator.script)
  const finsetCS = instance.nodeMP.hash
  const nodes = await Helpers.getNodeUtxos(lucid, finsetCS, nodeAddress)
  const pubKeys = nodes.map(getPKfromNode)
  const refundKeys = !winner
    ? pubKeys.filter((pk: string) => pk != "")
    : pubKeys.filter((pk: string) => pk != "" && pk != winner)
  const sortedKeys = refundKeys.sort((a, b) => (a > b ? -1 : 1))
  for (let i = 0; i < sortedKeys.length; ) {
    const bidder = sortedKeys.shift()!
    await refundBidder(instance, marketInstance, lucid, bidder)
    if (sortedKeys.length > 1) {
      const bidder2 = sortedKeys.pop()!
      await delay(10000)
      await refundBidder(instance, marketInstance, lucid, bidder2)
    }
    await delay(40000)
  }
  console.log("all lost bids have been refunded")
}

async function refundBidder(
  instance: CompiledAuctionInstance,
  marketInstance: CompiledMarketInstance,
  lucid: Lucid,
  bidder: string
) {
  console.log(`Refunding Bidder ${bidder}...`)
  const incompletedTx = await refund({ lucid, instance, marketInstance }, bidder)
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx`)
  return txHash
}

function delay(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms))
}

function getPKfromNode(node: [L.UTxO, Datums.SetNode]): string {
  const [_, datum] = node
  if (datum.key.kind == "Nothing") {
    return ""
  } else {
    return datum.key.pubKey
  }
}
