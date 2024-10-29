import { TxOutRef } from "../../plutus/plutus"
import * as S from "../types"

type NFTInstance<MP> = { nftMP: MP }

type RawCompiledNFTInstance = S.RawCompiledScript<"MintingPolicy">

export type CompiledNFTInstance = NFTInstance<S.CompiledScript<"MintingPolicy">>

const toLucidInstance = (instance: RawCompiledNFTInstance): CompiledNFTInstance => ({ nftMP: S.lucidRepr(instance) })

export async function makeNFTInstance(oref: TxOutRef): Promise<CompiledNFTInstance> {
  const address = `${S.scriptCompilerAddress}/nft/`
  const ref = new Blob([JSON.stringify(oref)], { type: "application/json" })
  const req = await ref.text()
  console.log(`Request: ${req}`)
  const response: Response = await fetch(
    new Request(address, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Request-Headers": "Content-Type",
      },
      body: ref,
    })
  )
  try {
    const json: RawCompiledNFTInstance = await response.clone().json()
    const instance: CompiledNFTInstance = toLucidInstance(json)

    console.log("Nft Script successfuly created")
    console.log(instance)
    return instance
  } catch (e) {
    const err: string = await response.clone().text()
    console.log(err)
    throw e
  }
}
