import { Lucid, Utils, OutputData, Tx } from "lucid-cardano"
import * as L from "lucid-cardano"
import * as Plutus from "../../plutus/plutus"
import * as Helpers from "../helpers"
import * as Schema from "../../data/schema"
import * as Contract from "../helpers/contract"
import { toLucidData } from "../../data/schema"
import { AuctionTerms } from "../types"
import { CompiledAuctionInstance, CompiledMarketInstance } from "../../services/types"
import {
  BidEscrow,
  MarketTerms,
  AuctionEscrow,
  SetNode,
  AuctionRedeemer,
  StateMPRedeemer,
  SetNodeRedeemer,
  unit,
} from "../../data/datums"
import { fromSchemaAddress, toSchemaAddress, toSchemaTerms } from "../../data/converstion"
import { scriptRefCS } from "../.."
import { RefundReason, Address, CancelReason } from "../../data/plutus-encoder"
import "../../data/datums"

export async function announceAuction(params: {
  auction: AuctionTerms
  lucid: Lucid
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const mkToken = Contract.MakeToken(params.instance)

  const auctionEscrow: AuctionEscrow = {
    kind: "AuctionEscrow",
    terms: toSchemaTerms(params.auction),
    nodeCS: Plutus.csOf(params.instance.nodeMP.hash),
  }
  const auctionDatum: OutputData = { inline: toLucidData(auctionEscrow) }
  const headNode: SetNode = { kind: "SetNode", key: { kind: "Nothing" }, next: { kind: "Nothing" } }
  const headNodeDatum: OutputData = { inline: toLucidData(headNode) }
  const announce: StateMPRedeemer = { kind: "Announce" }
  const init: SetNodeRedeemer = { kind: "Init" }

  const lotUtxo = await Helpers.getLot(params.lucid, params.auction)
  console.log(params.auction)

  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const marketDatum: MarketTerms = Helpers.getMarketDatum(marketUtxo)

  const nodeToken = mkToken.node.head(1n)
  const corrNodeToken = mkToken.node.corresponding(1n)
  const auctionToken = mkToken.auctionEscrow(1n)
  const minAda = Helpers.adaAsset(Helpers.tolovelace(marketDatum.minAda))
  const doubleMinAda = Helpers.adaAsset(2n * Helpers.tolovelace(marketDatum.minAda))
  const auctionValidator: L.Assets = {
    ...auctionToken,
    ...corrNodeToken,
    ...Plutus.valueToAssets(params.auction.lot),
    ...doubleMinAda,
  }

  return new Tx(params.lucid)
    .readFrom([marketUtxo])
    .collectFrom([lotUtxo])
    .attachMintingPolicy(params.instance.stateMP.script)
    .attachMintingPolicy(params.instance.nodeMP.script)
    .addSignerKey(params.auction.auctionInfo.seller.paymentCredential?.hash!)
    .payToContract(utils.validatorToAddress(params.instance.auctionValidator.script), auctionDatum, auctionValidator)
    .payToContract(utils.validatorToAddress(params.instance.nodeValidator.script), headNodeDatum, {
      ...nodeToken,
      ...minAda,
    })
    .mintAssets(auctionToken, Schema.toLucidData(announce))
    .mintAssets({ ...nodeToken, ...corrNodeToken }, Schema.toLucidData(init))
}

export async function cancelAuction(params: {
  lucid: Lucid
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)

  const auctionValidator = params.instance.auctionValidator.script
  const nodeVal = params.instance.nodeValidator.script
  const mkToken = Contract.MakeToken(params.instance)

  const auctionToken = (amount: bigint): L.Assets => mkToken.auctionEscrow(amount)
  const nodeToken = (amount: bigint): L.Assets => mkToken.node.head(amount)
  const corrNodeToken = mkToken.node.corresponding(-1n)
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken(1n), utils.validatorToAddress(auctionValidator))
  const nodeUtxos = await Helpers.getUtxos(params.lucid, nodeToken(1n), utils.validatorToAddress(nodeVal))
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)
  console.log(auctionEscrow)

  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const marketDatum: MarketTerms = Helpers.getMarketDatum(marketUtxo)

  const bought = Helpers.hasBoughtToken(auctionUtxo, params.instance.stateMP.hash)

  const scriptToken = mkToken.scriptEscrow(Plutus.cs(scriptRefCS))
  const scriptUtxo = await Helpers.getUtxo(params.lucid, scriptToken, utils.validatorToAddress(auctionValidator))

  const cancelReason: CancelReason = bought ? { kind: "Bought" } : { kind: "Failed" }

  const cancel: AuctionRedeemer = {
    kind: "CancelAct",
    cs: params.instance.stateMP.hash.unCurrencySymbol,
    reason: cancelReason,
  }
  const lifecycle: StateMPRedeemer = { kind: "LifeCycle" }
  const deinit: SetNodeRedeemer = { kind: "Deinit" }

  const doubleMinAda = Helpers.adaAsset(2n * Helpers.tolovelace(marketDatum.minAda))
  const lot = Plutus.schemaToAssets(auctionEscrow.terms.lot)
  const seller = auctionEscrow.terms.auctionInfo.seller
  const sellerAddr = fromSchemaAddress(utils, seller)

  const tx = new Tx(params.lucid)
    .readFrom([marketUtxo, scriptUtxo])
    .collectFrom([auctionUtxo], toLucidData(cancel))
    .collectFrom(nodeUtxos, unit())
    .attachMintingPolicy(params.instance.stateMP.script)
    .attachMintingPolicy(params.instance.nodeMP.script)
    .attachSpendingValidator(nodeVal)
    .mintAssets(auctionToken(-1n), toLucidData(lifecycle))
    .mintAssets({ ...nodeToken(-1n), ...corrNodeToken }, Schema.toLucidData(deinit))

  if (bought) {
    return tx.payToAddress(sellerAddr, doubleMinAda).mintAssets(mkToken.boughtEscrow(-1n), toLucidData(lifecycle))
  } else {
    return tx.payToAddress(sellerAddr, { ...lot, ...doubleMinAda }).addSignerKey(seller.paymentCredential.hash)
  }
}

export async function makeBid(params: {
  lucid: Lucid
  bidder: L.AddressDetails
  bid: bigint
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const bidder = params.bidder.paymentCredential?.hash!

  const auctionValidator = params.instance.auctionValidator.script
  const mkToken = Contract.MakeToken(params.instance)
  const auctionToken = mkToken.auctionEscrow(1n)
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionValidator))
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)

  const bidToken = mkToken.bidEscrow(1n)
  const bidUtxos = await Helpers.getUtxos(params.lucid, bidToken, utils.validatorToAddress(auctionValidator))
  const currentBidUtxo = Helpers.findBidUtxo(bidder, bidUtxos)

  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const marketDatum: MarketTerms = Helpers.getMarketDatum(marketUtxo)

  const scriptToken = mkToken.scriptEscrow(Plutus.cs(scriptRefCS))
  const scriptUtxo = await Helpers.getUtxo(params.lucid, scriptToken, utils.validatorToAddress(auctionValidator))

  const minAda = Helpers.tolovelace(marketDatum.minAda)
  const baseBid = Helpers.tolovelace(params.bid)
  const bidFees =
    auctionEscrow.terms.auctionInfo.sellerToCoverFees.kind === "True"
      ? 0n
      : (baseBid * marketDatum.percentageFee) / 100n + Helpers.tolovelace(marketDatum.fixedFee)
  const escrow = baseBid + bidFees
  const bidValue = Helpers.adaAsset(escrow + minAda)

  console.log(`Increasing bid to: ${Helpers.fromlovelace(baseBid)} Ada`)
  console.log(`Fees required if bid wins: ${Helpers.fromlovelace(bidFees)} Ada`)
  console.log(`Minimum escrow required: ${Helpers.fromlovelace(escrow)} Ada`)

  const slot = params.lucid.currentSlot()
  const now = utils.slotToUnixTime(slot)
  const close = Number(auctionEscrow.terms.auctionTime.close)
  const start = Number(auctionEscrow.terms.auctionTime.start)

  Helpers.assert(
    now < close || now > start,
    `The auction is not currently running. \n 
   The current time is: ${now} \n The auction runs from: ${start} to ${close}`
  )

  const bidEscrow: BidEscrow = {
    kind: "BidEscrow",
    status: { kind: "Bid", bid: params.bid, time: BigInt(now + Helpers.addBidTime) },
    bidder: toSchemaAddress(params.bidder),
  }
  const bidDatum: OutputData = { inline: toLucidData(bidEscrow) }

  const bidAct: AuctionRedeemer = { kind: "BidAct", cs: params.instance.stateMP.hash.unCurrencySymbol }

  return new Tx(params.lucid)
    .readFrom([marketUtxo, auctionUtxo, scriptUtxo])
    .collectFrom([currentBidUtxo!], toLucidData(bidAct))
    .addSignerKey(params.bidder.paymentCredential?.hash!)
    .validFrom(Helpers.afterTime(start))
    .validTo(now + Helpers.addTime)
    .payToContract(utils.validatorToAddress(auctionValidator), bidDatum, { ...bidToken, ...bidValue })
}

export async function extendAuction(params: {
  lucid: Lucid
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)

  const extend: AuctionRedeemer = { kind: "ExtendAct", cs: params.instance.stateMP.hash.unCurrencySymbol }

  const auctionValidator = params.instance.auctionValidator.script
  const mkToken = Contract.MakeToken(params.instance)
  const auctionUtxo = await Helpers.getUtxo(
    params.lucid,
    mkToken.auctionEscrow(1n),
    utils.validatorToAddress(auctionValidator)
  )

  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)

  const bidUtxos = await Helpers.getBidUtxos(
    params.lucid,
    params.instance.stateMP.hash,
    utils.validatorToAddress(auctionValidator)
  )
  const [bidUtxo, bidEscrow] = Helpers.sortBidsByTime(bidUtxos).pop()!

  const slot = params.lucid.currentSlot()
  const now = utils.slotToUnixTime(slot)
  const close = Number(auctionEscrow.terms.auctionTime.close)

  Helpers.assert(now < close, `The auction is has already finished`)

  const time = auctionEscrow.terms.auctionTime
  const bidTime = Helpers.getTime(bidEscrow)
  console.log(bidTime)
  const hasExtension = Helpers.getExtension(time.maybeExtension)
  const auctionWindow = time.close - hasExtension.extension.window
  Helpers.assert(
    bidTime > auctionWindow,
    `There are no bids within the extension window: \n
     The window opens at: ${auctionWindow}, \n
     The latest bid was made at: ${bidTime}`
  )

  const scriptToken = mkToken.scriptEscrow(Plutus.cs(scriptRefCS))
  const scriptUtxo = await Helpers.getUtxo(params.lucid, scriptToken, utils.validatorToAddress(auctionValidator))
  const outAuctionEscrow: AuctionEscrow = {
    kind: "AuctionEscrow",
    terms: {
      kind: "AuctionTerms",
      lot: auctionEscrow.terms.lot,
      auctionInfo: auctionEscrow.terms.auctionInfo,
      bidInfo: auctionEscrow.terms.bidInfo,
      auctionTime: {
        kind: "AuctionTime",
        start: time.start,
        close: time.close + hasExtension.extension.length,
        maybeExtension: time.maybeExtension,
      },
    },
    nodeCS: auctionEscrow.nodeCS,
  }
  const auctionDatum: OutputData = { inline: toLucidData(outAuctionEscrow) }

  return new Tx(params.lucid)
    .readFrom([marketUtxo, scriptUtxo])
    .readFrom([bidUtxo])
    .collectFrom([auctionUtxo], toLucidData(extend))
    .payToContract(utils.validatorToAddress(params.instance.auctionValidator.script), auctionDatum, auctionUtxo.assets)
}

// mostly a demo function. you may be looking for Helpers.findHighestBidder or Helpers.findHighestBid
export async function getHighestBid(params: {
  lucid: Lucid
  bidder: L.AddressDetails
  instance: CompiledAuctionInstance
}): Promise<void> {
  const utils = new Utils(params.lucid)
  const bidder = params.bidder.paymentCredential?.hash!

  console.log("Finding the highest bid")
  const auctionValidator = params.instance.auctionValidator.script
  const mkToken = Contract.MakeToken(params.instance)
  const auctionToken = mkToken.auctionEscrow(1n)
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionValidator))
  const bought = Helpers.hasBoughtToken(auctionUtxo, params.instance.stateMP.hash)
  Helpers.assert(!bought, `The auction lot has been bought`)
  const bidUtxos = await Helpers.getUtxos(
    params.lucid,
    mkToken.bidEscrow(1n),
    utils.validatorToAddress(auctionValidator)
  )
  console.log(`There are currently ${bidUtxos.length} bidders in the Auction`)

  const currentBidUtxo = Helpers.findBidUtxo(bidder, bidUtxos)
  const highestBid = Helpers.findHighestBid(bidUtxos)
  Helpers.assert(
    !!currentBidUtxo,
    `This address is not currently enrolled in the auction. \n
    The current highest bid is ${highestBid}`
  )
  const currentBidDatum: BidEscrow = Helpers.bidDatum(currentBidUtxo!)
  const currentBid = Helpers.getBid(currentBidDatum)
  if (currentBid === highestBid) {
    console.log(`You currently have the highest bid of ${currentBid} Ada`)
  } else {
    console.log(`The highest bid is currently ${highestBid} Ada`)
    console.log("Would you like to make a bid?")
  }
}

// mostly a demo function. you may be looking for Helpers.auctionIsResolved or Helpers.getBidUtxosFromParams
export async function getAuctionInfo(params: { lucid: Lucid; instance: CompiledAuctionInstance }): Promise<void> {
  const utils = new Utils(params.lucid)

  console.log("Gathering Auction Information")
  const auctionValidator = params.instance.auctionValidator.script
  const mkToken = Contract.MakeToken(params.instance)
  const auctionToken = mkToken.auctionEscrow(1n)
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionValidator))
  const bought = Helpers.hasBoughtToken(auctionUtxo, params.instance.stateMP.hash)
  Helpers.assert(!bought, `The auction lot has been bought`)
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)
  console.log(`The auction terms:`)
  console.log(auctionEscrow.terms)
  const slot = params.lucid.currentSlot()
  const now = utils.slotToUnixTime(slot)
  const close = Number(auctionEscrow.terms.auctionTime.close)
  const start = Number(auctionEscrow.terms.auctionTime.start)
  if (now < start) {
    console.log(`The auction has not started.`)
  } else if (now > close) {
    console.log(`The auction has finished.`)
  } else {
    console.log(`The Auction is currently running and ends at ${close}`)
    const bidUtxos = await Helpers.getUtxos(
      params.lucid,
      mkToken.bidEscrow(1n),
      utils.validatorToAddress(auctionValidator)
    )
    console.log(`There are currently ${bidUtxos.length} bidders in the Auction`)
    const highestBid = Helpers.findHighestBid(bidUtxos)
    console.log(`The highest bid is currently ${highestBid} Ada`)
  }
}

export async function refund(
  params: { lucid: Lucid; instance: CompiledAuctionInstance; marketInstance: CompiledMarketInstance },
  bidder: string
): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const auctionValidator = params.instance.auctionValidator.script
  const nodeVal = params.instance.nodeValidator.script
  const stateCS = params.instance.stateMP.hash
  const mkToken = Contract.MakeToken(params.instance)

  const auctionToken = mkToken.auctionEscrow(1n)
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionValidator))
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)

  const scriptToken = mkToken.scriptEscrow(Plutus.cs(scriptRefCS))
  const scriptUtxo = await Helpers.getUtxo(params.lucid, scriptToken, utils.validatorToAddress(auctionValidator))

  const slot = params.lucid.currentSlot()
  const now = utils.slotToUnixTime(slot)
  const close = Number(auctionEscrow.terms.auctionTime.close)

  // Find the current Bid
  const bidUtxos = await Helpers.getUtxos(
    params.lucid,
    mkToken.bidEscrow(1n),
    utils.validatorToAddress(auctionValidator)
  )
  const currentBidUtxo = Helpers.findBidUtxo(bidder, bidUtxos)
  const currentBidDatum: BidEscrow = Helpers.bidDatum(currentBidUtxo!)
  const currentBid = Helpers.getBid(currentBidDatum)

  // Find the Nodes
  const nodeValUtxos = await params.lucid.utxosAt(utils.validatorToAddress(nodeVal))
  const nodeCS = params.instance.nodeMP.hash
  const nodeUtxos = Helpers.findNodeUtxos(nodeCS, nodeValUtxos)
  const currentNodeUtxo = Helpers.findNodeWithKey(bidder, nodeUtxos)
  const currentNode = Helpers.nodeDatum(currentNodeUtxo)
  const prevNodeUtxo = Helpers.findNodeWithNext(bidder, nodeUtxos)
  const prevNode = Helpers.nodeDatum(prevNodeUtxo)
  const prevNodeAssets = prevNodeUtxo!.assets

  const currentNodeToken = mkToken.node.nonHead(bidder, -1n)

  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const bought = Helpers.hasBoughtToken(auctionUtxo, stateCS)

  const losingAssets = currentBidUtxo!.assets

  const refundReason: RefundReason = bought ? { kind: "BoughtLot" } : { kind: "LostBid" }
  const updatedNode: SetNode = { kind: "SetNode", key: prevNode.key, next: currentNode.next }
  const updatedNodeDatum: OutputData = { inline: toLucidData(updatedNode) }
  const refund: AuctionRedeemer = {
    kind: "RefundAct",
    cs: params.instance.stateMP.hash.unCurrencySymbol,
    reason: refundReason,
  }
  const lifecycle: StateMPRedeemer = { kind: "LifeCycle" }
  const remove: SetNodeRedeemer = { kind: "Remove", pk: bidder, node: updatedNode }

  const refundValue = Helpers.lovelaceValueOf(losingAssets)

  const tx = new Tx(params.lucid)
    .readFrom([marketUtxo, auctionUtxo, scriptUtxo])
    .collectFrom([currentBidUtxo!], toLucidData(refund))
    .collectFrom([currentNodeUtxo, prevNodeUtxo], unit())
    .attachSpendingValidator(nodeVal)
    .attachMintingPolicy(params.instance.stateMP.script)
    .attachMintingPolicy(params.instance.nodeMP.script)
    .payToContract(utils.validatorToAddress(nodeVal), updatedNodeDatum, prevNodeAssets)
    .payToAddress(fromSchemaAddress(utils, currentBidDatum.bidder), Helpers.adaAsset(refundValue))
    .mintAssets(mkToken.bidEscrow(-1n), toLucidData(lifecycle))
    .mintAssets(currentNodeToken, toLucidData(remove))

  if (refundReason.kind === "LostBid" && currentBidDatum.status.kind === "Bid") {
    const higherBidUtxo = Helpers.findHigherBidUtxo(currentBid, bidUtxos)
    Helpers.assert(!!higherBidUtxo, "There is no higher bid")
    tx.readFrom([higherBidUtxo!])
  } else {
    tx
  }

  if (bought) {
    tx
  } else if (now > close) {
    tx.validFrom(Helpers.afterTime(close))
  } else {
    tx.addSignerKey(bidder)
  }

  return tx
}

export async function resolve(params: {
  lucid: Lucid
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const mkToken = Contract.MakeToken(params.instance)
  const auctionValidator = params.instance.auctionValidator.script
  const auctionUtxo = await Helpers.getUtxo(
    params.lucid,
    mkToken.auctionEscrow(1n),
    utils.validatorToAddress(auctionValidator)
  )
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)

  const bidUtxos = await Helpers.getUtxos(
    params.lucid,
    mkToken.bidEscrow(1n),
    utils.validatorToAddress(auctionValidator)
  )
  Helpers.assert(bidUtxos.length == 1, "All bids have not been resolved")
  const currentBidUtxo = bidUtxos[0]!
  const currentBid: BidEscrow = Helpers.bidDatum(currentBidUtxo)
  const bidEscrowAdaVal = Helpers.lovelaceValueOf(currentBidUtxo.assets)

  const nodeVal = params.instance.nodeValidator.script
  const nodeValUtxos = await params.lucid.utxosAt(utils.validatorToAddress(nodeVal))
  const nodeCS = params.instance.nodeMP.hash
  const nodeUtxos = Helpers.findNodeUtxos(nodeCS, nodeValUtxos)

  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const marketDatum: MarketTerms = Helpers.getMarketDatum(marketUtxo)

  const scriptToken = mkToken.scriptEscrow(Plutus.cs(scriptRefCS))
  const scriptUtxo = await Helpers.getUtxo(params.lucid, scriptToken, utils.validatorToAddress(auctionValidator))

  const originNode = mkToken.node.head(-1n)
  const winningNode = mkToken.node.nonHead(currentBid.bidder.paymentCredential.hash, -1n)
  const corrNodeToken = mkToken.node.corresponding(-1n)

  const bid = Helpers.tolovelace(Helpers.getBid(currentBid))
  const change = bidEscrowAdaVal - bid
  const minAda = Helpers.tolovelace(marketDatum.minAda)
  const fixedFee = Helpers.tolovelace(marketDatum.fixedFee)
  const percentageFee = (bid * marketDatum.percentageFee) / 100n
  const fees = fixedFee + percentageFee
  const lot = Plutus.schemaToAssets(auctionEscrow.terms.lot)

  const slot = params.lucid.currentSlot()
  const now = utils.slotToUnixTime(slot)
  const close = Number(auctionEscrow.terms.auctionTime.close)

  // check the auction has finished
  Helpers.assert(
    now > close,
    `The auction has not finished. \n The current time is: 
    ${now}\n The auction ends at: ${close}`
  )

  const lifecycle: StateMPRedeemer = { kind: "LifeCycle" }
  const removeAndDeinit: SetNodeRedeemer = { kind: "RemoveAndDeinit", pk: currentBid.bidder.paymentCredential.hash }
  const resolve: AuctionRedeemer = { kind: "ResolveAct", cs: params.instance.stateMP.hash.unCurrencySymbol }

  const base = { toBidder: change, toBeneficiaries: 2n * minAda + bid }
  const { toBidder, toBeneficiaries } =
    auctionEscrow.terms.auctionInfo.sellerToCoverFees.kind === "True"
      ? { ...base, toBeneficiaries: base.toBeneficiaries - fees }
      : { ...base, toBidder: base.toBidder - fees }

  const tx = new Tx(params.lucid)
    .readFrom([marketUtxo, scriptUtxo])
    .collectFrom(nodeUtxos, unit())
    .collectFrom([currentBidUtxo, auctionUtxo], toLucidData(resolve))
    .attachSpendingValidator(nodeVal)
    .attachMintingPolicy(params.instance.stateMP.script)
    .attachMintingPolicy(params.instance.nodeMP.script)
    .validFrom(Helpers.afterTime(close))
    .payToAddress(fromSchemaAddress(utils, marketDatum.address), Helpers.adaAsset(fees))
    .payToAddress(fromSchemaAddress(utils, currentBid.bidder), { ...lot, ...Helpers.adaAsset(toBidder) })
    .mintAssets({ ...mkToken.auctionEscrow(-1n), ...mkToken.bidEscrow(-1n) }, toLucidData(lifecycle))
    .mintAssets({ ...originNode, ...winningNode, ...corrNodeToken }, toLucidData(removeAndDeinit))

  function payBens(ben: Map<Address, bigint>, tx: Tx) {
    const bens = Array.from(ben)
    bens.forEach(([ben, percentage]) =>
      tx.payToAddress(fromSchemaAddress(utils, ben), Helpers.adaAsset((toBeneficiaries / 100n) * percentage))
    )
    return tx
  }

  return payBens(auctionEscrow.terms.auctionInfo.beneficiaries, tx)
}

export async function buyNow(params: {
  lucid: Lucid
  bidder: L.AddressDetails
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const bidder = params.bidder.paymentCredential?.hash!
  const mkToken = Contract.MakeToken(params.instance)
  const auctionValidator = params.instance.auctionValidator.script
  const auctionToken = mkToken.auctionEscrow(1n)
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionValidator))
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)

  const bidUtxos = await Helpers.getUtxos(
    params.lucid,
    mkToken.bidEscrow(1n),
    utils.validatorToAddress(auctionValidator)
  )
  const currentBidUtxo = Helpers.findBidUtxo(bidder, bidUtxos)

  const nodeVal = params.instance.nodeValidator.script
  const nodeCS = params.instance.nodeMP.hash
  const nodeValUtxos = await params.lucid.utxosAt(utils.validatorToAddress(nodeVal))
  const nodeUtxos = Helpers.findNodeUtxos(nodeCS, nodeValUtxos)
  const currentNodeUtxo = Helpers.findNodeWithKey(bidder, nodeUtxos)
  const currentNode = Helpers.nodeDatum(currentNodeUtxo)
  const prevNodeUtxo = Helpers.findNodeWithNext(bidder, nodeUtxos)
  const prevNode = Helpers.nodeDatum(prevNodeUtxo)
  const prevNodeAssets = prevNodeUtxo!.assets

  const currentNodeToken = mkToken.node.nonHead(bidder, -1n)

  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const marketDatum: MarketTerms = Helpers.getMarketDatum(marketUtxo)

  const scriptToken = mkToken.scriptEscrow(Plutus.cs(scriptRefCS))
  const scriptUtxo = await Helpers.getUtxo(params.lucid, scriptToken, utils.validatorToAddress(auctionValidator))

  const doubleMinAda = Helpers.adaAsset(Helpers.tolovelace(2n * marketDatum.minAda))
  const auctionValue: L.Assets = {
    ...auctionToken,
    ...mkToken.node.corresponding(1n),
    ...mkToken.boughtEscrow(1n),
    ...doubleMinAda,
  }

  const buyNowPrice = Helpers.tolovelace(auctionEscrow.terms.bidInfo.buyNowPrice)
  const minAda = Helpers.tolovelace(marketDatum.minAda)
  const fixedFee = Helpers.tolovelace(marketDatum.fixedFee)
  const percentageFee = (buyNowPrice / 100n) * marketDatum.percentageFee
  const fees = fixedFee + percentageFee
  const lot = Plutus.schemaToAssets(auctionEscrow.terms.lot)

  const updatedNode: SetNode = { kind: "SetNode", key: prevNode.key, next: currentNode.next }
  const updatedNodeDatum: OutputData = { inline: toLucidData(updatedNode) }
  const lifecycle: StateMPRedeemer = { kind: "LifeCycle" }
  const remove: SetNodeRedeemer = { kind: "Remove", pk: bidder, node: updatedNode }
  const auctionDatum: OutputData = { inline: toLucidData(auctionEscrow) }
  const buyNow: AuctionRedeemer = { kind: "BuyNowAct", cs: params.instance.stateMP.hash.unCurrencySymbol }
  console.log(fees)
  console.log()
  const base = 2n * minAda + buyNowPrice
  const toBeneficiaries = auctionEscrow.terms.auctionInfo.sellerToCoverFees.kind === "True" ? base - fees : base

  const tx = new Tx(params.lucid)
    .readFrom([marketUtxo, scriptUtxo])
    .collectFrom([currentNodeUtxo, prevNodeUtxo], unit())
    .collectFrom([currentBidUtxo!, auctionUtxo], toLucidData(buyNow))
    .attachSpendingValidator(nodeVal)
    .addSignerKey(params.bidder.paymentCredential?.hash!)
    .attachMintingPolicy(params.instance.stateMP.script)
    .attachMintingPolicy(params.instance.nodeMP.script)
    .payToContract(utils.validatorToAddress(auctionValidator), auctionDatum, auctionValue)
    .payToContract(utils.validatorToAddress(nodeVal), updatedNodeDatum, prevNodeAssets)
    .payToAddress(fromSchemaAddress(utils, marketDatum.address), Helpers.adaAsset(fees))
    .payToAddress(params.bidder.address.bech32, { ...lot, ...Helpers.adaAsset(minAda) })
    .mintAssets({ ...mkToken.bidEscrow(-1n), ...mkToken.boughtEscrow(1n) }, toLucidData(lifecycle))
    .mintAssets({ ...currentNodeToken }, toLucidData(remove))

  function payBens(ben: Map<Address, bigint>, tx: Tx) {
    const bens = Array.from(ben)
    bens.forEach(([ben, percentage]) =>
      tx.payToAddress(fromSchemaAddress(utils, ben), Helpers.adaAsset((toBeneficiaries / 100n) * percentage))
    )
    return tx
  }

  return payBens(auctionEscrow.terms.auctionInfo.beneficiaries, tx)
}
