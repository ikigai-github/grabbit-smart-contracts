import { Lucid, Utils, Data, Tx, OutputData } from "lucid-cardano"
import * as L from "lucid-cardano"
import * as Helpers from "../helpers"
import * as Opt from "fp-ts/lib/Option"
import * as Contract from "../helpers/contract"
import * as Plutus from "../../plutus/plutus"
import {
  BidEscrow,
  MarketTerms,
  SetNode,
  StateMPRedeemer,
  SetNodeRedeemer,
  AuctionEscrow,
  unit,
  AuctionRedeemer,
} from "../../data/datums"
import { toLucidData, toPlutusData } from "../../data/schema"
import { CompiledAuctionInstance, CompiledMarketInstance } from "../../services/types"
import { BidStatus, Address } from "../../data/plutus-encoder"
import { Option } from "fp-ts/lib/Option"
import { fromSchemaAddress, toSchemaAddress } from "../../data/converstion"
import "../../data/datums"
import { scriptRefCS } from "../.."

export async function enroll(
  params: {
    lucid: Lucid
    enrollee: L.AddressDetails
    instance: CompiledAuctionInstance
    marketInstance: CompiledMarketInstance
  },
  withBid: Option<bigint>
): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const ePKH = params.enrollee.paymentCredential?.hash!
  const mkToken = Contract.MakeToken(params.instance)

  const auctionVal = params.instance.auctionValidator.script
  const nodeVal = params.instance.nodeValidator.script
  const auctionCS = params.instance.stateMP.hash
  const nodeCS = params.instance.nodeMP.hash

  // Tokens
  const auctionToken = mkToken.auctionEscrow(1n)
  const bidToken = mkToken.bidEscrow(1n)
  const insertNodeToken = mkToken.node.nonHead(ePKH, 1n)

  // UTxOs
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionVal))
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)

  const nodeValUtxos = await params.lucid.utxosAt(utils.validatorToAddress(nodeVal))
  const nodeUtxos = Helpers.findNodeUtxos(nodeCS, nodeValUtxos)
  const coveringNodeUtxo = Helpers.findCoveringNode(ePKH, nodeUtxos)

  const slot = params.lucid.currentSlot()
  const now = utils.slotToUnixTime(slot)
  const close = Number(auctionEscrow.terms.auctionTime.close)
  const start = Number(auctionEscrow.terms.auctionTime.start)

  // Datums
  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const marketDatum: MarketTerms = Helpers.getMarketDatum(marketUtxo)

  const { bidStatus, escrow } = Opt.match<bigint, { bidStatus: BidStatus; escrow: bigint }>(
    () => ({ bidStatus: { kind: "NoBid" }, escrow: Helpers.tolovelace(auctionEscrow.terms.bidInfo.startingPrice) }),
    (bid) => {
      Helpers.assert(now > start, `The auction has not started yet no bids may be placed`)
      const baseBid = Helpers.tolovelace(bid)
      const bidFees =
        auctionEscrow.terms.auctionInfo.sellerToCoverFees.kind === "True"
          ? 0n
          : (baseBid * marketDatum.percentageFee) / 100n + Helpers.tolovelace(marketDatum.fixedFee)
      const escrow = baseBid + bidFees
      console.log(`Enrolling with bid of: ${Helpers.fromlovelace(baseBid)} Ada`)
      console.log(`Fees required if bid wins: ${Helpers.fromlovelace(bidFees)} Ada`)
      console.log(`Minimum escrow required: ${Helpers.fromlovelace(escrow)} Ada`)
      return { bidStatus: { kind: "Bid", bid: bid, time: BigInt(now + Helpers.addBidTime) }, escrow: escrow }
    }
  )(withBid)

  // check the auction has not finished
  Helpers.assert(
    now < close,
    `The auction has finished. \n The current time is: ${now} \n 
      The auction ended at: ${close}`
  )

  Helpers.assert(!!coveringNodeUtxo, `This pubKeyHash has already been enrolled`)
  const coveringNode: SetNode = Helpers.nodeDatum(coveringNodeUtxo!)

  const insertNode: SetNode = { kind: "SetNode", key: { kind: "JustPKH", pubKey: ePKH }, next: coveringNode.next }
  const insertNodeDatum: OutputData = { inline: Data.to(toPlutusData(insertNode)) }
  const updatedNode: SetNode = { kind: "SetNode", key: coveringNode.key, next: { kind: "JustPKH", pubKey: ePKH } }
  const updatedNodeDatum: OutputData = { inline: Data.to(toPlutusData(updatedNode)) }
  const bidEscrow: BidEscrow = { kind: "BidEscrow", status: bidStatus, bidder: toSchemaAddress(params.enrollee) }
  const bidDatum: OutputData = { inline: Data.to(toPlutusData(bidEscrow)) }

  // Redeemers
  const mintBidToken: StateMPRedeemer = { kind: "Enroll" }
  const insert: SetNodeRedeemer = { kind: "Insert", pk: ePKH, node: coveringNode }

  // Value
  const minAda = Helpers.tolovelace(marketDatum.minAda)

  const tx = new Tx(params.lucid)
    .readFrom([auctionUtxo, marketUtxo])
    .collectFrom([coveringNodeUtxo!], unit())
    .attachMintingPolicy(params.instance.stateMP.script)
    .attachMintingPolicy(params.instance.nodeMP.script)
    .attachSpendingValidator(nodeVal)
    .addSignerKey(params.enrollee.paymentCredential?.hash!)
    .payToContract(
      utils.validatorToAddress(auctionVal), //, params.enrollee.stakeCredential) // removed for simplicity. kept for reference when we finish query layer.
      bidDatum,
      { ...bidToken, ...Helpers.adaAsset(escrow + minAda) }
    )
    .payToContract(utils.validatorToAddress(nodeVal), insertNodeDatum, {
      ...insertNodeToken,
      ...Helpers.adaAsset(minAda),
    })
    .payToContract(utils.validatorToAddress(nodeVal), updatedNodeDatum, { ...coveringNodeUtxo!.assets })
    .mintAssets(bidToken, Data.to(toPlutusData(mintBidToken)))
    .mintAssets(insertNodeToken, Data.to(toPlutusData(insert)))

  if (withBid._tag == "None") {
    return tx.validTo(now + Helpers.addTime)
  } else {
    return tx.validFrom(Helpers.beforeTime(now)).validTo(now + Helpers.addTime)
  }
}

export async function buyNowNoEnroll(params: {
  lucid: Lucid
  buyer: L.AddressDetails
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const buyer: Address = toSchemaAddress(params.buyer)
  const mkToken = Contract.MakeToken(params.instance)
  const auctionValidator = params.instance.auctionValidator.script
  const auctionToken = mkToken.auctionEscrow(1n)
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionValidator))
  const auctionEscrow: AuctionEscrow = Helpers.getAuctionDatum(auctionUtxo)

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

  const lifecycle: StateMPRedeemer = { kind: "LifeCycle" }
  const auctionDatum: OutputData = { inline: toLucidData(auctionEscrow) }
  const buyNow: AuctionRedeemer = { kind: "BuyNowNoEnrollAct", cs: params.instance.stateMP.hash.unCurrencySymbol, receiver: buyer }
  console.log(fees)
  console.log()
  const base = 2n * minAda + buyNowPrice
  const toBeneficiaries = auctionEscrow.terms.auctionInfo.sellerToCoverFees.kind === "True" ? base - fees : base

  const tx = new Tx(params.lucid)
    .readFrom([marketUtxo, scriptUtxo])
    .collectFrom([auctionUtxo], toLucidData(buyNow))
    .addSignerKey(params.buyer.paymentCredential?.hash!)
    .attachMintingPolicy(params.instance.stateMP.script)
    .payToContract(utils.validatorToAddress(auctionValidator), auctionDatum, auctionValue)
    .payToAddress(fromSchemaAddress(utils, marketDatum.address), Helpers.adaAsset(fees))
    .payToAddress(params.buyer.address.bech32, { ...lot, ...Helpers.adaAsset(minAda) })
    .mintAssets({ ...mkToken.boughtEscrow(1n) }, toLucidData(lifecycle))

  function payBens(ben: Map<Address, bigint>, tx: Tx) {
    const bens = Array.from(ben)
    bens.forEach(([ben, percentage]) =>
      tx.payToAddress(fromSchemaAddress(utils, ben), Helpers.adaAsset((toBeneficiaries / 100n) * percentage))
    )
    return tx
  }

  return payBens(auctionEscrow.terms.auctionInfo.beneficiaries, tx)
}