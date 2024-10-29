{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Vulcan.Onchain.Collections.OnchainFTMint where

import GHC.Stack (HasCallStack)
import Plutarch (Config (Config), TracingMode (DoTracing))
import Plutarch.Api.V1 (PCredential (..), PValidatorHash)
import Plutarch.Api.V1.Value (padaToken, pnormalize, pvalueOf)
import Plutarch.Api.V2 (
  PCurrencySymbol,
  PMintingPolicy,
  POutputDatum (POutputDatum),
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PMinting, PSpending),
  PTokenName (..),
  PTxOut,
  PTxOutRef,
  PValidator,
  mintingPolicySymbol,
  mkMintingPolicy,
 )
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
 )
import Plutarch.Extra.ScriptContext (pfromPDatum)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
  ptraceC,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Num ((#+))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  MintingPolicy,
  PubKeyHash,
  TxOutRef,
  ValidatorHash,
 )
import PlutusTx qualified

import Vulcan.Onchain.Collections.Utils (
  phasInput,
  pnegativeSymbolValueOf,
  ppositiveSymbolValueOf,
  (#>),
 )

data FTMintAction
  = MintFT
  | BurnFT
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''FTMintAction

data PFTMintAction (s :: S)
  = PMintFT (Term s (PDataRecord '[]))
  | PBurnFT (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PFTMintAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PFTMintAction where
  type PLifted PFTMintAction = FTMintAction

deriving via
  (DerivePConstantViaData FTMintAction PFTMintAction)
  instance
    (PConstantDecl FTMintAction)

-- This minting policy makes the following checks:
-- Minting requires the spending of a UTxO with a TxOutRef (which is a parameter to the script)
-- You cannot mint and burn in the same transaction
-- Burning is allowed for anyone who owns the asset.
pvalidateFTMint :: Term s (PTxOutRef :--> PFTMintAction :--> PScriptContext :--> PUnit)
pvalidateFTMint = phoistAcyclic $
  plam $ \oref redm context -> unTermCont $ do
    contextFields <- tcont (pletFields @'["txInfo", "purpose"] context)

    PMinting policy <- pmatchC contextFields.purpose

    ownPolicyId <- pletC $ pfield @"_0" # policy

    txInfoFields <- tcont (pletFields @'["inputs", "mint"] contextFields.txInfo)

    mintedValue <- pletC txInfoFields.mint
    mintedRTs <- pletC $ ppositiveSymbolValueOf # ownPolicyId # mintedValue
    burnedRTs <- pletC $ pnegativeSymbolValueOf # ownPolicyId # mintedValue
    txIns <- pletC txInfoFields.inputs
    let isUtxoSpent = phasInput # txIns # oref

    pure $
      pif
        ( pmatch
            redm
            ( \case
                PMintFT _ -> mintedRTs #> 0 #&& burnedRTs #== 0 #&& isUtxoSpent
                PBurnFT _ -> mintedRTs #== 0 #&& burnedRTs #< 0
            )
        )
        (pconstant ())
        perror

pmintFTPolicyW :: Term s (PTxOutRef :--> PMintingPolicy)
pmintFTPolicyW = phoistAcyclic $
  plam $ \oref redm ctx ->
    let redeemer :: Term _ PFTMintAction
        redeemer = punsafeCoerce redm
     in popaque $ pvalidateFTMint # oref # redeemer # ctx

pmetadataControlFTW :: Term s (PAsData PCurrencySymbol :--> PValidator)
pmetadataControlFTW = phoistAcyclic $
  plam $ \cs _dat _redm _ctx -> popaque $ pif (cs #== cs) perror perror

ftMintPolicyH :: Config -> TxOutRef -> MintingPolicy
ftMintPolicyH cfg oref =
  mkMintingPolicy cfg (pmintFTPolicyW # pconstant oref)

ftMintCurrencySymbolH :: Config -> TxOutRef -> CurrencySymbol
ftMintCurrencySymbolH cfg oref = mintingPolicySymbol $ ftMintPolicyH cfg oref
