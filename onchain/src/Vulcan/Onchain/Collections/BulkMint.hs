{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Vulcan.Onchain.Collections.BulkMint where

import Plutarch (Config)
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
 )
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PScriptPurpose (PMinting),
  PTxOutRef,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  MintingPolicy,
  TxOutRef,
 )
import PlutusTx qualified
import Vulcan.Onchain.Collections.Utils (
  phasInput,
  pnegativeSymbolValueOf,
  ppositiveSymbolValueOf,
  (#>),
 )

data BulkMintAction
  = MintBulk
  | BurnBulk
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''BulkMintAction

data PBulkMintAction (s :: S)
  = PMintBulk (Term s (PDataRecord '[]))
  | PBurnBulk (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PBulkMintAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PBulkMintAction where
  type PLifted PBulkMintAction = BulkMintAction

deriving via
  (DerivePConstantViaData BulkMintAction PBulkMintAction)
  instance
    (PConstantDecl BulkMintAction)

data BulkMintParametersD = BulkMintParametersD
  { uniqueRef :: TxOutRef
  , collectionSize :: Integer
  }
PlutusTx.unstableMakeIsData ''BulkMintParametersD

data PBulkMintParametersD (s :: S)
  = PBulkMintParameters
      ( Term
          s
          ( PDataRecord
              '[ "uniqueRef" ':= PTxOutRef
               , "collectionSize" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PBulkMintParametersD where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PBulkMintParametersD where
  type PLifted PBulkMintParametersD = BulkMintParametersD

deriving via
  (DerivePConstantViaData BulkMintParametersD PBulkMintParametersD)
  instance
    (PConstantDecl BulkMintParametersD)

-- This minting policy makes the following checks:
-- Minting requires the spending of a UTxO with a TxOutRef (which is a parameter to the script)
-- Exactly collectionSize (from the BulkMintParameters) tokens must be minted when minting.
-- You cannot mint and burn in the same transaction
-- Burning is allowed for anyone who owns the asset.
pvalidateBulkMint :: Term s (PBulkMintParametersD :--> PBulkMintAction :--> PScriptContext :--> PUnit)
pvalidateBulkMint = phoistAcyclic $
  plam $ \bulkMintParameters redm context -> unTermCont $ do
    bmParamsF <- pletFieldsC @'["uniqueRef", "collectionSize"] bulkMintParameters
    contextFields <- tcont (pletFields @'["txInfo", "purpose"] context)

    PMinting policy <- pmatchC contextFields.purpose

    ownPolicyId <- pletC $ pfield @"_0" # policy

    txInfoFields <- tcont (pletFields @'["inputs", "mint"] contextFields.txInfo)

    mintedValue <- pletC txInfoFields.mint
    mintedRTs <- pletC $ ppositiveSymbolValueOf # ownPolicyId # mintedValue
    burnedRTs <- pletC $ pnegativeSymbolValueOf # ownPolicyId # mintedValue
    txIns <- pletC txInfoFields.inputs
    let isUtxoSpent = phasInput # txIns # bmParamsF.uniqueRef

    pure $
      pif
        ( pmatch
            redm
            ( \case
                PMintBulk _ -> mintedRTs #== bmParamsF.collectionSize #&& burnedRTs #== 0 #&& isUtxoSpent
                PBurnBulk _ ->
                  let checkOrigin =
                        pall
                          # plam
                            ( \txIn ->
                                plet (pfield @"resolved" # txIn) $ \txInOut ->
                                  pif
                                    (0 #< ppositiveSymbolValueOf # ownPolicyId # (pfield @"value" # txInOut))
                                    ( pmatch (pfield @"credential" # (pfield @"address" # txInOut)) $ \case
                                        PPubKeyCredential _ -> pconstant True
                                        PScriptCredential _ -> pconstant False
                                    )
                                    (pconstant True)
                            )
                          # txIns
                   in mintedRTs #== 0 #&& burnedRTs #< 0 #&& checkOrigin
            )
        )
        (pconstant ())
        perror

pbulkMintPolicyW :: Term s (PBulkMintParametersD :--> PMintingPolicy)
pbulkMintPolicyW = phoistAcyclic $
  plam $ \bmParams redm ctx ->
    -- let redeemer = unsafeCoerce @BulkMintAction redm in
    let redeemer :: Term _ PBulkMintAction
        redeemer = punsafeCoerce redm
     in popaque $ pvalidateBulkMint # bmParams # redeemer # ctx

bulkMintPolicyH :: Config -> BulkMintParametersD -> MintingPolicy
bulkMintPolicyH cfg bmParams =
  mkMintingPolicy cfg (pbulkMintPolicyW # pconstant bmParams)

bulkMintCurrencySymbolH :: Config -> BulkMintParametersD -> CurrencySymbol
bulkMintCurrencySymbolH cfg bulkMintParams = mintingPolicySymbol $ bulkMintPolicyH cfg bulkMintParams
