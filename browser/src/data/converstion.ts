import * as L from "lucid-cardano"
import * as Plutus from "../plutus/plutus"
import * as Datums from "./datums"
import * as Helpers from "../contracts/helpers"
import { AuctionTerms } from "../contracts/types"
import { AuctionInfo, BidInfo, AuctionTime } from "./datums"
import { Boolean, Address, MaybeStakingCredential } from "./plutus-encoder"

export function toSchemaAddress(n: L.AddressDetails): Address {
  return {
    kind: `Address`,
    paymentCredential: { kind: `PubKeyCredential`, hash: n.paymentCredential?.hash! },
    stakingCredential: toStaking(n),
  }
}
export const addressPkh = (addr: Address): Plutus.PubKeyHash => Plutus.pkh(addr.paymentCredential.hash)

const toStaking = (n: L.AddressDetails): MaybeStakingCredential =>
  typeof n.stakeCredential === undefined
    ? { kind: `Nothing` }
    : {
        kind: "JustStakingCredential",
        stakingCredential: {
          kind: `StakingHash`,
          credential: { kind: `PubKeyCredential`, hash: n.stakeCredential?.hash! },
        },
      }

export function fromSchemaAddress(utils: L.Utils, n: Address): L.Address {
  const keyHash = utils.keyHashToCredential(n.paymentCredential.hash!)
  const stake = fromStaking(utils, n.stakingCredential)
  return utils.credentialToAddress(keyHash, stake)
}

const fromStaking = (utils: L.Utils, n: MaybeStakingCredential): L.Credential | undefined =>
  n.kind === "Nothing"
    ? undefined
    : n.stakingCredential.kind == "StakingPtr"
    ? undefined
    : utils.keyHashToCredential(n.stakingCredential.credential.hash)

export function toSchemaTerms(auctionTerms: AuctionTerms): Datums.AuctionTerms {
  const sellerCovers: Boolean = auctionTerms.auctionInfo.sellerToCoverFees ? { kind: "True" } : { kind: "False" }

  const getTns = (map: Map<Plutus.TokenName, bigint>): Map<string, bigint> =>
    Helpers.mapMapKVs((tn: Plutus.TokenName, amount: bigint) => [tn.unTokenName, amount])(map)

  const auctionLot: Map<string, Map<string, bigint>> = Helpers.mapMapKVs(
    (cs: Plutus.CurrencySymbol, tokens: Map<Plutus.TokenName, bigint>) => [cs.unCurrencySymbol, getTns(tokens)]
  )(auctionTerms.lot)

  const bens: Map<Address, bigint> = Helpers.mapMapKVs((ben: L.AddressDetails, percentage: bigint) => [
    toSchemaAddress(ben),
    percentage,
  ])(auctionTerms.auctionInfo.beneficiaries)

  const extension: Datums.MaybeExtension = !auctionTerms.time.extension
    ? { kind: "Nothing" }
    : {
        kind: "JustExtension",
        extension: {
          kind: "TimeExtension",
          window: auctionTerms.time.extension!.window,
          length: auctionTerms.time.extension!.length,
        },
      }

  const aInfo: AuctionInfo = {
    kind: "AuctionInfo",
    seller: toSchemaAddress(auctionTerms.auctionInfo.seller),
    beneficiaries: bens,
    sellerToCoverFees: sellerCovers,
  }
  const bInfo: BidInfo = {
    kind: "BidInfo",
    buyNowPrice: auctionTerms.bidInfo.buyNowPrice,
    startingPrice: auctionTerms.bidInfo.startingPrice,
    raiseMinimum: auctionTerms.bidInfo.raiseMinimum,
    raisePercentage: auctionTerms.bidInfo.raisePercentage,
  }

  const tInfo: AuctionTime = {
    kind: "AuctionTime",
    start: BigInt(auctionTerms.time.start.getPOSIXTime),
    close: BigInt(auctionTerms.time.close.getPOSIXTime),
    maybeExtension: extension,
  }
  return { kind: "AuctionTerms", lot: auctionLot, auctionInfo: aInfo, bidInfo: bInfo, auctionTime: tInfo }
}
