import { Data, Lucid, Tx, AddressDetails } from "lucid-cardano"
import * as P from "../../plutus/plutus"
import { NFTInfo } from "../types"
import { NFTMetadata } from "./types"
import { CompiledNFTInstance } from "../../services/nft-script-compiler"
import * as H from "../helpers"

/* NFT minting policy parameterized by a wallets TxOutRef.
  NB vulcan-server must be running 
*/
export async function mintNFT(params: {
  info: NFTInfo
  lucid: Lucid
  wallet: AddressDetails
  instance: CompiledNFTInstance
}): Promise<Tx> {
  const Redeemer = () => Data.void()
  const auctionLot: P.Value = H.singleton(params.instance.nftMP.hash, params.info.tokenName, 1n)
  const lot = P.valueToAssets(auctionLot)
  const utxos = await params.lucid.utxosAt(params.wallet.address.bech32)
  const key = 721
  const policy = P.csOf(params.instance.nftMP.hash)
  const name = P.tnOf(params.info.tokenName)
  const metadata: NFTMetadata = {
    [policy]: {
      [name]: params.info.meta,
    },
  }

  console.log(metadata)

  return new Tx(params.lucid)
    .collectFrom(utxos)
    .attachMintingPolicy(params.instance.nftMP.script)
    .attachMetadata(key, metadata)
    .payToAddress(params.wallet.address.bech32, { ...lot })
    .mintAssets(lot, Redeemer())
}
