import { Lucid, Utils, Tx, OutputData } from "lucid-cardano"
import * as L from "lucid-cardano"
import * as Plutus from "../../plutus/plutus"
import * as Helpers from "../helpers"
import * as Schema from "../../data/schema"
import {
  BidEscrow,
  RegistrationEscrow,
  MarketTerms,
  SetNode,
  AuctionRedeemer,
  StateMPRedeemer,
  SetNodeRedeemer,
  unit,
} from "../../data/datums"
import { CompiledAuctionInstance, CompiledMarketInstance } from "../../services/types"
import { scriptRefCS } from "../.."
import { toSchemaAddress } from "../../data/converstion"
import * as Contract from "../helpers/contract"

export async function registerInterest(params: {
  lucid: Lucid
  registrant: L.AddressDetails
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const mkToken = Contract.MakeToken(params.instance)
  const registration: RegistrationEscrow = { kind: "Registration", registrant: toSchemaAddress(params.registrant) }
  const regDatum: OutputData = { inline: Schema.toLucidData(registration) }
  const register: StateMPRedeemer = { kind: "Enroll" }

  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const marketDatum: MarketTerms = Helpers.getMarketDatum(marketUtxo)

  const auctionVal = params.instance.auctionValidator.script
  const auctionToken = mkToken.auctionEscrow(1n)
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionVal))
  const auctionEscrow = Helpers.getAuctionDatum(auctionUtxo!)
  const minAda = Helpers.tolovelace(marketDatum.minAda * 2n)
  const startingPrice = Helpers.tolovelace(auctionEscrow.terms.bidInfo.startingPrice)
  const regValue = Helpers.adaAsset(minAda + startingPrice)
  const regToken = mkToken.registrationEscrow(1n)

  return new Tx(params.lucid)
    .readFrom([marketUtxo])
    .attachMintingPolicy(params.instance.stateMP.script)
    .payToContract(utils.validatorToAddress(params.instance.auctionValidator.script), regDatum, {
      ...regValue,
      ...regToken,
    })
    .mintAssets(regToken, Schema.toLucidData(register))
}

export async function unRegister(params: {
  lucid: Lucid
  registrant: L.AddressDetails
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const rPKH = params.registrant.paymentCredential?.hash!
  const mkToken = Contract.MakeToken(params.instance)

  const unRegister: AuctionRedeemer = { kind: "UnRegisterAct", cs: params.instance.stateMP.hash.unCurrencySymbol }
  const lifecycle: StateMPRedeemer = { kind: "LifeCycle" }

  const auctionVal = params.instance.auctionValidator.script

  const scriptToken = mkToken.scriptEscrow(Plutus.cs(scriptRefCS))
  const scriptUtxo = await Helpers.getUtxo(params.lucid, scriptToken, utils.validatorToAddress(auctionVal))
  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)

  const regUtxos = await Helpers.getUtxos(
    params.lucid,
    mkToken.registrationEscrow(1n),
    utils.validatorToAddress(auctionVal)
  )
  const regUtxo = Helpers.findRegUtxo(rPKH, regUtxos)!
  const refund = Helpers.lovelaceValueOf(regUtxo.assets)

  return new Tx(params.lucid)
    .readFrom([marketUtxo, scriptUtxo])
    .attachMintingPolicy(params.instance.stateMP.script)
    .addSignerKey(params.registrant.paymentCredential?.hash!)
    .collectFrom([regUtxo], Schema.toLucidData(unRegister))
    .payToAddress(params.registrant.address.bech32, Helpers.adaAsset(refund))
    .mintAssets(mkToken.registrationEscrow(-1n), Schema.toLucidData(lifecycle))
}

export async function issuePaddle(params: {
  lucid: Lucid
  enrollee: L.AddressDetails
  instance: CompiledAuctionInstance
  marketInstance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const ePKH = params.enrollee.paymentCredential?.hash!
  const mkToken = Contract.MakeToken(params.instance)

  const auctionVal = params.instance.auctionValidator.script
  const nodeVal = params.instance.nodeValidator.script
  const nodeCS = params.instance.nodeMP.hash

  const scriptToken = mkToken.scriptEscrow(Plutus.cs(scriptRefCS))
  const scriptUtxo = await Helpers.getUtxo(params.lucid, scriptToken, utils.validatorToAddress(auctionVal))

  // Tokens
  const auctionToken = mkToken.auctionEscrow(1n)
  const bidToken = mkToken.bidEscrow(1n)
  const insertNodeToken = mkToken.node.nonHead(ePKH, 1n)

  // UTxOs
  const auctionUtxo = await Helpers.getUtxo(params.lucid, auctionToken, utils.validatorToAddress(auctionVal))
  const auctionEscrow = Helpers.getAuctionDatum(auctionUtxo!)

  const nodeValUtxos = await params.lucid.utxosAt(utils.validatorToAddress(nodeVal))
  const nodeUtxos = Helpers.findNodeUtxos(nodeCS, nodeValUtxos)
  const coveringNodeUtxo = Helpers.findCoveringNode(ePKH, nodeUtxos)

  const regUtxos = await Helpers.getUtxos(
    params.lucid,
    mkToken.registrationEscrow(1n),
    utils.validatorToAddress(auctionVal)
  )
  const regUtxo = Helpers.findRegUtxo(ePKH, regUtxos)!
  const regAmount = Helpers.lovelaceValueOf(regUtxo.assets)

  const slot = params.lucid.currentSlot()
  const now = utils.slotToUnixTime(slot)
  const close = Number(auctionEscrow.terms.auctionTime.close)

  // check the auction has not finished
  Helpers.assert(
    now > close,
    `The auction has finished. \n The current time is: ${now} 
    \n The auction ended at: ${close}`
  )

  // Datums
  const marketUtxo = await Helpers.getMarketUTxO(params.lucid, params.marketInstance)
  const marketDatum: MarketTerms = Helpers.getMarketDatum(marketUtxo)

  const coveringNode: SetNode = Helpers.nodeDatum(coveringNodeUtxo!)

  const insertNode: SetNode = { kind: "SetNode", key: { kind: "JustPKH", pubKey: ePKH }, next: coveringNode.next }
  const insertNodeDatum: OutputData = { inline: Schema.toLucidData(insertNode) }
  const updatedNode: SetNode = { kind: "SetNode", key: coveringNode.key, next: { kind: "JustPKH", pubKey: ePKH } }
  const updatedNodeDatum: OutputData = { inline: Schema.toLucidData(updatedNode) }
  const bidEscrow: BidEscrow = {
    kind: "BidEscrow",
    status: { kind: "NoBid" },
    bidder: toSchemaAddress(params.enrollee),
  }
  const bidDatum: OutputData = { inline: Schema.toLucidData(bidEscrow) }

  // Redeemers
  const lifecycle: StateMPRedeemer = { kind: "LifeCycle" }
  const insert: SetNodeRedeemer = { kind: "Insert", pk: ePKH, node: coveringNode }
  const issuePaddle: AuctionRedeemer = { kind: "IssuePaddle", cs: params.instance.stateMP.hash.unCurrencySymbol }

  // Value
  const minAda = Helpers.tolovelace(marketDatum.minAda)

  return new Tx(params.lucid)
    .readFrom([marketUtxo, auctionUtxo, scriptUtxo])
    .collectFrom([coveringNodeUtxo!], unit())
    .collectFrom([regUtxo], Schema.toLucidData(issuePaddle))
    .attachMintingPolicy(params.instance.stateMP.script)
    .attachMintingPolicy(params.instance.nodeMP.script)
    .attachSpendingValidator(nodeVal)
    .validTo(close - Helpers.addTime)
    .addSignerKey(auctionEscrow.terms.auctionInfo.seller.paymentCredential?.hash!)
    .payToContract(utils.validatorToAddress(auctionVal), bidDatum, {
      ...bidToken,
      ...Helpers.adaAsset(regAmount - minAda),
    })
    .payToContract(utils.validatorToAddress(nodeVal), insertNodeDatum, {
      ...insertNodeToken,
      ...Helpers.adaAsset(minAda),
    })
    .payToContract(utils.validatorToAddress(nodeVal), updatedNodeDatum, coveringNodeUtxo!.assets)
    .mintAssets({ ...bidToken, ...mkToken.registrationEscrow(-1n) }, Schema.toLucidData(lifecycle))
    .mintAssets(insertNodeToken, Schema.toLucidData(insert))
}
