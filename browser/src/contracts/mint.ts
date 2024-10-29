import { Data, MintingPolicy, Lucid, Tx, AddressDetails } from "lucid-cardano"
import * as P from "../plutus/plutus"
import * as H from "./helpers"

// A simple always succeeds minting policy

const mintingScript: MintingPolicy = {
  type: "PlutusV1",
  script: "4746010000222261",
}

const CS = "7865710ee3b580f02787f3e5830f78ef6e23ed75cf53c304dfce562a"

export const auctionLot: P.Value = H.singleton(P.cs(CS), P.tn("LotToken"), 1n)

export async function mintToken(lucid: Lucid, seller: AddressDetails): Promise<Tx> {
  const Redeemer = () => Data.void()
  const lot = P.valueToAssets(auctionLot)

  const utxos = await lucid.utxosAt(seller.address.bech32)

  return new Tx(lucid)
    .collectFrom(utxos)
    .attachMintingPolicy(mintingScript)
    .payToAddress(seller.address.bech32, { ...lot })
    .mintAssets(lot, Redeemer())
}
