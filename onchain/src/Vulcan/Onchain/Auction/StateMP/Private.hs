module Vulcan.Onchain.Auction.StateMP.Private (
  mkPrivStateMP,
  privStateMP,
  pPrivStateMP,
) where

import Plutarch.Api.V1 (
  PCurrencySymbol,
 )
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PTxInInfo,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  MintingPolicy,
  TxOutRef,
  ValidatorHash,
 )

import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Onchain.Auction.StateMP.Common (
  PStateCommon (cs, minAdaValue, mint, toValidator),
  makeCommon,
  pAnnounceAuction,
  plifeCycle,
 )
import Vulcan.SpecialUTxO.Types (pnameToken)
import Vulcan.SpecialUTxO.Utils (parseRegistrationOutput)
import Vulcan.Types.Auction (PRegistrationEscrow)
import Vulcan.Types.State (
  PPrivStateMintingRedeemer (
    PAnnounceAuction,
    PAuctionLifeCycle,
    PRegistration
  ),
 )
import Vulcan.Utils (
  passert,
  pverifyDataC,
  toPlutarchConfig,
  wrapMintingPolicy,
 )

---------------------------
-- Minting Policy:
---------------------------

mkPrivStateMP ::
  Vulcan.Config ->
  ValidatorHash ->
  CurrencySymbol ->
  ClosedTerm
    ( PCurrencySymbol
        :--> PTxOutRef
        :--> PPrivStateMintingRedeemer
        :--> PScriptContext
        :--> PUnit
    )
mkPrivStateMP cfg auctionValHash marketCS = plam $ \nodeCS ref redm ctx ->
  pmatch redm $ \case
    PAuctionLifeCycle _ -> plifeCycle cfg # pconstant auctionValHash # ctx
    otherRedeemers -> P.do
      (common, _, _, _) <-
        runTermCont $
          makeCommon auctionValHash marketCS nodeCS ctx
      case otherRedeemers of
        PAnnounceAuction _ -> pAnnounceAuction cfg common # ref
        -- this redeemer is unique for Private auction
        PRegistration _ -> P.do
          -- Outputs:
          -- created RegistrationEscrow UTxO
          _ <-
            plet $
              parseRegistrationOutput
                # common.minAdaValue
                # common.cs
                # common.toValidator
          passert cfg "Only a registration token may be minted" $
            let registrationToken =
                  Value.psingleton
                    # common.cs
                    # pnameToken @PRegistrationEscrow
                    # 1
             in common.mint #== registrationToken
          pconstant ()

pPrivStateMP ::
  Vulcan.Config ->
  ValidatorHash ->
  CurrencySymbol ->
  ClosedTerm
    ( PAsData PCurrencySymbol
        :--> PTxOutRef
        :--> PMintingPolicy
    )
pPrivStateMP cfg auctionVH pmarketCS =
  plam $ \nodeCS oref redm' ctx -> unTermCont $ do
    redm <- pverifyDataC @PPrivStateMintingRedeemer redm'
    pure . popaque $
      mkPrivStateMP cfg auctionVH pmarketCS
        # pfromData nodeCS
        # oref
        # pfromData redm
        # ctx

-- | Compile the State Token Minting Policy
privStateMP ::
  Vulcan.Config ->
  ValidatorHash ->
  CurrencySymbol ->
  CurrencySymbol ->
  TxOutRef ->
  MintingPolicy
privStateMP config auctionVH pmarketCS nodeCS oref =
  wrapMintingPolicy (toPlutarchConfig config) $
    mkPrivStateMP config auctionVH pmarketCS
      # pconstant nodeCS
      # pconstant oref
