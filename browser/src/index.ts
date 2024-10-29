import * as L from "lucid-cardano"
import * as Plutus from "./plutus/plutus"
import * as Helpers from "./contracts/helpers"
import * as Vulcan from "./contracts/types"
import * as Mint from "./contracts/mint"
import * as Opt from "fp-ts/lib/Option"
import * as Contract from "./contracts/helpers/contract"
import { AuctionParams } from "./contracts/types"
import {
  announceAuction,
  makeBid,
  cancelAuction,
  resolve,
  refund,
  getHighestBid,
  buyNow,
  getAuctionInfo,
  extendAuction,
} from "./contracts/auction/common"
import { Blockfrost, Lucid, Utils } from "lucid-cardano"
import { getInstance, getMarketInstance, makeInstance, makeMarketInstance } from "./services/script-compiler"
import { CompiledAuctionInstance, CompiledMarketInstance } from "./services/types"
import { issuePaddle, registerInterest, unRegister } from "./contracts/auction/private"
import { buyNowNoEnroll, enroll } from "./contracts/auction/public"
import { mintMarket } from "./contracts/market/mint"
import { scriptUtxo } from "./contracts/market/reference"
import { refundNodes } from "./contracts/refundAll"
import ENV from "../env.json"
import { createSingleMintTransaction } from "./contracts/nft/bulk"

console.log("Server running")
const blockfrostUrl = "https://cardano-preview.blockfrost.io/api/v0"

const projectId = ENV.PROJECT_ID

const bf: Blockfrost = new Blockfrost(blockfrostUrl, projectId)
export const lucid: Lucid = await Lucid.new(bf, "Preview")
const utils: Utils = new Utils(lucid)

// Assumes you are in a browser environment - enables interfacing with Nami.
console.log("Created new Lucid instance")

export const scriptRefCS = "6ad442d55ef1f8a752fc32470440d2c1c3c4e6193d433a1a1b6249ef"

const mintBtn = document.getElementById("mint")!
const mintNFTBtn = document.getElementById("mint-nft")!
const marketBtn = document.getElementById("mint-market")!
const scriptBtn = document.getElementById("script-market")!
const updateMarketBtn = document.getElementById("update-market")!
const extendBtn = document.getElementById("extend-auction")!
const useCalculatedTimeBtn = document.getElementById("use-calculated-time")!
const announceBtn = document.getElementById("announce")!
const cancelBtn = document.getElementById("cancel")!
const registerBtn = document.getElementById("register")!
const unRegisterBtn = document.getElementById("un-register")!
const issuePaddleBtn = document.getElementById("issue-paddle")!
const enrollBtn = document.getElementById("enroll")!
const enrollBidBtn = document.getElementById("enroll-bid")!
const bidStatusBtn = document.getElementById("bid-status")!
const auctionStatusBtn = document.getElementById("auction-status")!
const bidBtn = document.getElementById("bid")!
const buyNowBtn = document.getElementById("buy-now")!
const buyNowNoEnrollBtn = document.getElementById("buy-now-no-enroll")!

//const refundBtn = document.getElementById("refund")!;
const getRefundBtn = document.getElementById("get-refund")!
const refundAllBtn = document.getElementById("refund-all")!
const resolveBtn = document.getElementById("resolve")!
const resolve2Btn = document.getElementById("resolve2")!

const price = (): bigint => Field.bigint("price")
const buyNowPrice = (): bigint => Field.bigint("buy-now-price")
const bidIncFixed = (): bigint => Field.bigint("bid-inc-fixed")
const bidIncPerc = (): bigint => Field.bigint("bid-inc-perc")
const startTime = (): Plutus.POSIXTime => Field.posixTime("starting-time")
const closeTime = (): Plutus.POSIXTime => Field.posixTime("closing-time")
const delayBeforeAuction = (): number => Field.int("delay-before-auction")
const auctionDuration = (): number => Field.int("auction-duration")
const extensionWindow = (): bigint => Field.bigint("extension-window")
const extensionTime = (): bigint => Field.bigint("extension-time")
//@ts-ignore
const isPrivate = (): boolean => document.getElementById("is-private").checked
//@ts-ignore
const isExtendable = (): boolean => document.getElementById("extandable").checked
//@ts-ignore
const sellerToCover = (): boolean => document.getElementById("seller-covers").checked

const regAddress = (): L.Address => Field.lucidAddress("reg-address")
const nftName = (): string => Field.string("nft-name")
const tokenName = (): string => Field.string("token-name")
const nftImage = (): string => Field.string("nft-image")
const nftDescription = (): string => Field.string("nft-description")

//@ts-ignore
const nftCS = () => document.getElementById("cs")!.value
//@ts-ignore
const nftTN = () => document.getElementById("tn")!.value

const bidAmount = (): bigint => Field.bigint("bid-amount")
const enrollBidAmount = (): bigint => Field.bigint("enroll-bid-amount")
const marketAddress = (): L.Address => Field.lucidAddress("market-fee-addr")

namespace Field {
  export const int = (fieldName: string): number => parseInt(any(fieldName)!)
  export const bigint = (fieldName: string): bigint => BigInt(int(fieldName))
  export const string = (fieldName: string): string => any(fieldName)
  export const posixTime = (fieldName: string): Plutus.POSIXTime => Plutus.timeOf(int(fieldName))
  export const lucidAddress = (fieldName: string): L.Address => any(fieldName)
  export const any = <T>(fieldName: string): T => {
    //@ts-ignore
    const val: T = document.getElementById(fieldName)?.value!
    Helpers.assert(val != undefined, `Field "${fieldName}" value is not defined`)
    return val
  }
}

const calculateAuctionTime = (): Vulcan.AuctionTime => {
  const currentTime = Date.now()
  const minToMs = (min: number): number => min * 60_000
  const startingTime = currentTime + minToMs(delayBeforeAuction())
  const closingTime = startingTime + minToMs(auctionDuration())
  return { start: { getPOSIXTime: startingTime }, close: { getPOSIXTime: closingTime } }
}
useCalculatedTimeBtn.onclick = () => {
  const time: Vulcan.AuctionTime = calculateAuctionTime()
  //@ts-ignore
  document.getElementById("starting-time")!.value = time.start.getPOSIXTime
  //@ts-ignore
  document.getElementById("closing-time")!.value = time.close.getPOSIXTime
}

// collects params from web page fields
const getParams = (): Omit<AuctionParams, "oref"> => ({
  isPrivate: isPrivate(),
  config: { tracingMode: { tag: "DoTracing", contents: "Conscise" } },
})

mintBtn.onclick = async function () {
  console.log("Minting the Lot Token..")
  //@ts-ignore
  const wallet = await window.cardano.nami.enable()
  lucid.selectWallet(wallet)
  const walletAddr = await wallet.getUsedAddresses()
  const seller = utils.getAddressDetails(walletAddr[0]!)
  const incompletedTx = await Mint.mintToken(lucid, seller)
  const tx = await incompletedTx.complete()
  const signed = await tx.sign().complete()
  console.log(`Signed`)
  const hash = signed.submit()
  console.log(`Submitted Tx`)
}

mintNFTBtn.onclick = async function () {
  console.log(`Minting the NFT...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const wallet = utils.getAddressDetails(walletAddr[0]!)

  const getMetadata = (): L.NFTMetadataDetails => ({
    name: nftName(),
    image: "ipfs://" + nftImage(),
    mediaType: "image/jpeg",
    description: nftDescription(),
    files: [{ name: nftName(), mediaType: "image/jpeg", src: "ipfs://" + nftImage() }],
  })
  
  const tx = await createSingleMintTransaction(lucid, tokenName(), getMetadata())
  if(tx.error == undefined) {
    console.log(`Generated Tx`)
    const signed = await tx.tx.sign().complete()
    console.log(`Signed Tx`)
    const hash = await signed.submit()
    console.log(`Submitted Tx ${hash}`)
  }
  else {
    throw new Error(tx.error)
  }
}

marketBtn.onclick = async function () {
  console.log(`Generating the market...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const wallet = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledMarketInstance = await makeMarketInstance()
  const incompletedTx = await mintMarket({ lucid, wallet, instance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const hash = signed.submit()
  console.log(`Submitted Tx`)
}

scriptBtn.onclick = async function () {
  console.log(`Generating the auction script UTxO...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const auctionInstance: CompiledAuctionInstance = await getInstance()
  const incompletedTx = await scriptUtxo({ lucid, auctionInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const hash = signed.submit()
  console.log(`Submitted Tx`)
}

announceBtn.onclick = async function () {
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const seller = utils.getAddressDetails(walletAddr[0]!)
  const auctionInfo = (): Vulcan.AuctionInfo => ({
    seller: seller,
    beneficiaries: new Map<L.AddressDetails, bigint>([[seller, 100n]]),
    sellerToCoverFees: sellerToCover(),
  })
  const extension = (): Vulcan.TimeExtension | undefined => {
    if (isExtendable()) {
      return { window: 60_000n * extensionWindow(), length: 60_000n * extensionTime() }
    } else {
      return undefined
    }
  }

  const auctionTime = (): Vulcan.AuctionTime => ({ start: startTime(), close: closeTime(), extension: extension() })

  const bidInfo = (): Vulcan.BidInfo => ({
    buyNowPrice: buyNowPrice(),
    startingPrice: price(),
    raiseMinimum: bidIncFixed(),
    raisePercentage: bidIncPerc(),
  })

  const lotCS = nftCS()
  const lotTN = nftTN()
  const auctionLot: Plutus.Value = Helpers.singleton(Plutus.cs(lotCS), Plutus.tn(lotTN), 1n)
  const auctionTerms = (): Vulcan.AuctionTerms => ({
    lot: auctionLot,
    auctionInfo: auctionInfo(),
    bidInfo: bidInfo(),
    time: auctionTime(),
  })
  console.log(`Announcing The Auction...`)
  const auctionDetails = getParams()
  const auction = auctionTerms()
  const lotUtxo = await Helpers.getLot(lucid, auction)
  const instance: CompiledAuctionInstance = makeInstance({ ...auctionDetails, oref: Helpers.utxoOutRef(lotUtxo) })
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await announceAuction({ auction, lucid, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

cancelBtn.onclick = async function () {
  console.log(`Cancelling The Auction...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await cancelAuction({ lucid, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

registerBtn.onclick = async function () {
  console.log(`Registering for Auction...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const registrant = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await registerInterest({ registrant, lucid, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx${txHash}`)
  return txHash
}

unRegisterBtn.onclick = async function () {
  console.log(`de-Registering from auction...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const registrant = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await unRegister({ registrant, lucid, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

issuePaddleBtn.onclick = async function () {
  console.log(`Issueing Paddle...`)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const enrollee: L.AddressDetails = utils.getAddressDetails(regAddress())
  const incompletedTx = await issuePaddle({ enrollee, lucid, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

enrollBtn.onclick = async function () {
  console.log(`Enrolling...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const enrollee = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await enroll({ enrollee, lucid, instance, marketInstance }, Opt.none)
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

enrollBidBtn.onclick = async function () {
  console.log(`Enrolling with bid...`)
  //@ts-ignore
  const wallet = await window.cardano.nami.enable()
  lucid.selectWallet(wallet)
  const walletAddr = await wallet.getUsedAddresses()
  const enrollee = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const bid = enrollBidAmount()
  const incompletedTx = await enroll({ enrollee, lucid, instance, marketInstance }, Opt.some(bid))
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

bidStatusBtn.onclick = async function () {
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const bidder = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledAuctionInstance = await getInstance()
  getHighestBid({ lucid, bidder, instance })
}

auctionStatusBtn.onclick = async function () {
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const instance: CompiledAuctionInstance = await getInstance()
  getAuctionInfo({ lucid, instance })
}

bidBtn.onclick = async function () {
  console.log(`Bidding...`)
  //@ts-ignore
  const wallet = await window.cardano.nami.enable()
  lucid.selectWallet(wallet)
  const walletAddr = await wallet.getUsedAddresses()
  const bidder = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const bid: bigint = bidAmount()
  const incompletedTx = await makeBid({ lucid, bidder, bid, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

buyNowBtn.onclick = async function () {
  console.log(`Buying now...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const bidder = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await buyNow({ lucid, bidder, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

buyNowNoEnrollBtn.onclick = async function () {
  console.log(`Buying now...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const buyer = utils.getAddressDetails(walletAddr[0]!)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await buyNowNoEnroll({ lucid, buyer, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

extendBtn.onclick = async function () {
  console.log(`Extending the Auction...`)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await extendAuction({ lucid, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

//refundBtn.onclick = refundFirst
getRefundBtn.onclick = refundBidder

export async function refundAll() {
  console.log(`Refunding all bidders...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const auctionAddress = utils.validatorToAddress(instance.auctionValidator.script)
  const auctionCS = instance.stateMP.hash
  const mkToken = Contract.MakeToken(instance)
  const auctionToken = mkToken.auctionEscrow(1n)
  const auctionUtxo = await Helpers.getUtxo(lucid, auctionToken, auctionAddress)
  const auctionEscrow = Helpers.getAuctionDatum(auctionUtxo)
  const bought = Helpers.hasBoughtToken(auctionUtxo, auctionCS)
  const bidUtxos = await Helpers.getBidUtxos(lucid, auctionCS, auctionAddress)
  const slot = lucid.currentSlot()
  const now = utils.slotToUnixTime(slot)
  const close = Number(auctionEscrow.terms.auctionTime.close)

  // check the auction has finished
  Helpers.assert(
    now > close || bought,
    `The auction has not finished. \n The current time is: 
    ${now}\n The auction ends at: ${close}`
  )
  console.log(`There are currently ${bidUtxos.length} bidders enrolled in the Auction`)
  const sortedbidUtxos = Helpers.sortBids(bidUtxos)
  console.log("Determining the winner")
  const [_, winningBid] = sortedbidUtxos.pop()!
  const hasWinner: boolean = winningBid.status.kind === "Bid"
  if (bought) {
    console.log("The lot has been bought. Refunding all bidders")
    refundNodes(instance, marketInstance, lucid, utils)
  } else if (hasWinner) {
    const winner = winningBid.bidder.paymentCredential.hash
    console.log(`PubKeyHash: ${winner} has won
    the auction with a bid of: ${Helpers.getBid(winningBid)}`)
    refundNodes(instance, marketInstance, lucid, utils, winner)
  } else {
    console.log("There were no winning bids placed. Refunding all bidders")
    refundNodes(instance, marketInstance, lucid, utils)
  }
}

refundAllBtn.onclick = refundAll

// refunds the requested bid
async function refundBidder() {
  console.log(`Refunding Bidder...`)
  //@ts-ignore
  const walletApi = await window.cardano.nami.enable()
  lucid.selectWallet(walletApi)
  const walletAddr = await walletApi.getUsedAddresses()
  const bidAddress = utils.getAddressDetails(walletAddr[0]!)
  const bidder = bidAddress.paymentCredential?.hash!
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await refund({ lucid, instance, marketInstance }, bidder)
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}

resolveBtn.onclick = resolveAuction

async function resolveAuction() {
  console.log(`Resolving Auction...`)
  const instance: CompiledAuctionInstance = await getInstance()
  const marketInstance: CompiledMarketInstance = await getMarketInstance()
  const incompletedTx = await resolve({ lucid, instance, marketInstance })
  const tx = await incompletedTx.complete()
  console.log(`Generated Tx`)
  const signed = await tx.sign().complete()
  console.log(`Signed Tx`)
  const txHash = await signed.submit()
  console.log(`Submitted Tx ${txHash}`)
  return txHash
}
