import { Data, Lucid, Tx, Utils, type OutputData, type Script, getAddressDetails } from "lucid-cardano"
import * as Plutus from "../../plutus/plutus"
import type { CompiledAuctionInstance } from "../../services/types"
import * as Helpers from "../helpers"
import { createBulkMintPolicy } from "../nft/bulk"

export async function scriptUtxo(params: { lucid: Lucid; auctionInstance: CompiledAuctionInstance }): Promise<Tx> {
  const utils = new Utils(params.lucid)
  const admin = getAddressDetails(await params.lucid.wallet.address())

  const utxos = await params.lucid.utxosAt(admin.address.bech32)
  if (utxos.length === 0 || utxos[0] === undefined) {
    throw new Error("no utxos at admin address")
  }
  const nftMP: Script = createBulkMintPolicy(utxos[0], 1n)
  const nftMPHash: Plutus.CurrencySymbol = { unCurrencySymbol: utils.mintingPolicyToId(nftMP) }

  const Redeemer = () => Data.void()
  const refToken: Plutus.Value = Helpers.singleton(nftMPHash, Plutus.tn("AuctionScript"), 1n)
  const token = Plutus.valueToAssets(refToken)

  const auctionVal = params.auctionInstance.auctionValidator.script

  const scriptRef: OutputData = { inline: Data.void(), scriptRef: auctionVal }

  console.log("remember to update scriptRefCS to: " + nftMPHash.unCurrencySymbol)

  return new Tx(params.lucid)
    .collectFrom(utxos)
    .attachMintingPolicy(nftMP)
    .payToContract(utils.validatorToAddress(auctionVal), scriptRef, token)
    .mintAssets(token, Redeemer())
}
