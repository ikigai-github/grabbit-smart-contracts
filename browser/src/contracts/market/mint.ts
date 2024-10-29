import { Data, Lucid, Utils, Tx, AddressDetails, OutputData } from "lucid-cardano"
import { MarketTerms } from "../../data/datums"
import * as Schema from "../../data/schema"
import * as Plutus from "../../plutus/plutus"
import { CompiledMarketInstance } from "../../services/types"
import * as Helpers from "../helpers"
import { toSchemaAddress } from "../../data/converstion"

/* NFT minting policy parameterized by a wallets TxOutRef.
  NB vulcan-server must be running 
*/
export async function mintMarket(params: {
  lucid: Lucid
  wallet: AddressDetails
  instance: CompiledMarketInstance
}): Promise<Tx> {
  const utils = new Utils(params.lucid)

  const utxos = await params.lucid.utxosAt(params.wallet.address.bech32)

  const marketToken = Helpers.singletonAsset(params.instance.marketMP.hash, Plutus.tn("MarketEscrow"), 1n)
  const marketVal = params.instance.marketValidator.script
  const marketTerms: MarketTerms = {
    kind: `MarketTerms`,
    fixedFee: 2n,
    percentageFee: 5n,
    address: toSchemaAddress(params.wallet),
    minAda: 2n,
  }
  const marketData = Data.to(Schema.toPlutusData(marketTerms))
  const marketDatum: OutputData = { inline: marketData }

  const minAda = Helpers.adaAsset(marketTerms.minAda * 1_000_000n)

  return new Tx(params.lucid)
    .collectFrom(utxos)
    .attachMintingPolicy(params.instance.marketMP.script)
    .payToContract(utils.validatorToAddress(marketVal), marketDatum, { ...marketToken, ...minAda })
    .mintAssets(marketToken, marketData)
}
