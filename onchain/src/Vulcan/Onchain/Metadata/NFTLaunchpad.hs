{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Vulcan.Onchain.Metadata.NFTLaunchpad where

import GHC.Stack (HasCallStack)
import Plutarch (Config (Config), TracingMode (DoTracing))
import Plutarch.Api.V1 (PCredential (..), PValidatorHash)
import Plutarch.Api.V1.Value (padaToken, pnoAdaValue, pnormalize, pvalueOf)
import Plutarch.Api.V2 (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PAddress (..),
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
  PValue (..),
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
  ptryFromC,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Num ((#+))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  Address (..),
  CurrencySymbol,
  MintingPolicy,
  PubKeyHash,
  TxOutRef,
  ValidatorHash,
  Value (..),
 )
import PlutusTx qualified
import Vulcan.Onchain.Collections.Utils (
  PTriple (PTriple),
  pand'List,
  pcond,
  phasInput,
  ponlyAsset,
  ptryLookupValue,
  ptryOutputToAddress,
  ptryOwnInput,
  ptryOwnOutput,
  (#>),
 )
import Vulcan.Onchain.Metadata.Constants
import Vulcan.Onchain.Metadata.Types (PMetadataDatum (..), PMetadataEvolveDatum)

data EvolveAct
  = EvolveNFT
  | FundEvolution
  | Reclaim
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''EvolveAct

data PEvolveAct (s :: S)
  = PEvolveNFT (Term s (PDataRecord '[]))
  | PFundEvolution (Term s (PDataRecord '[]))
  | PReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PEvolveAct where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PEvolveAct where
  type PLifted PEvolveAct = EvolveAct

deriving via
  (DerivePConstantViaData EvolveAct PEvolveAct)
  instance
    (PConstantDecl EvolveAct)

instance PTryFrom PData (PAsData PEvolveAct)
instance PTryFrom PData PEvolveAct

data NFTMintAction
  = MintNFT
  | BurnNFT
  deriving stock (Generic, Show)

PlutusTx.unstableMakeIsData ''NFTMintAction

data PNFTMintAction (s :: S)
  = PMintNFT (Term s (PDataRecord '[]))
  | PBurnNFT (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PNFTMintAction where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PNFTMintAction where
  type PLifted PNFTMintAction = NFTMintAction

deriving via
  (DerivePConstantViaData NFTMintAction PNFTMintAction)
  instance
    (PConstantDecl NFTMintAction)

instance PTryFrom PData (PAsData PNFTMintAction)
instance PTryFrom PData PNFTMintAction

-- This minting policy makes the following checks:
-- Minting requires the spending of a UTxO with a TxOutRef (which is a parameter to the script)
-- You cannot mint and burn in the same transaction
-- Burning is allowed for anyone who owns the asset.
pvalidateNFTMint :: Term s (PNFTMintConfig :--> PNFTMintAction :--> PScriptContext :--> PUnit)
pvalidateNFTMint = phoistAcyclic $
  plam $ \launchConfig redm context -> unTermCont $ do
    launchConfigF <- pletFieldsC @'[ "initUTxO"
                                   , "threadToken"
                                   , "marketplaceFee"
                                   , "marketplaceAddress"
                                   , "projectFee"
                                   , "projectAddress"
                                   ] 
                                   launchConfig 
    contextFields <- pletFieldsC @'["txInfo", "purpose"] context

    PMinting policy <- pmatchC contextFields.purpose
    ownPolicyId <- pletC $ pfield @"_0" # policy

    txInfoFields <- tcont (pletFields @'["inputs", "outputs", "mint"] contextFields.txInfo)
    txIns <- pletC txInfoFields.inputs

    tkPairs <- pletC $ ptryLookupValue # ownPolicyId # txInfoFields.mint
    numMinted <- pletC (plength # tkPairs)

    let isUtxoSpent = phasInput # txIns # launchConfig.initUTxO

    pure $
      pif
        ( pmatch
            redm
            ( \case
                PMintNFT _ ->
                  let mintNFTCheck = unTermCont $ do
                        refPair <- pletC (phead # tkPairs)
                        userPair <- pletC (ptryIndex 1 tkPairs)
                        PPair refLabel refTkName <- pmatchC (pbreakTokenName (pfromData $ pfstBuiltin # refPair))
                        PPair userLabel userTkName <- pmatchC (pbreakTokenName (pfromData $ pfstBuiltin # userPair))
                        let hasThread = 
                              plam (\txIn -> 
                                ppositiveSymbolValueOf 
                                  # ownPlaunchConfigF.threadToken 
                                  # pfield @"value" # (pfield @"resolved" # txIn) #>= 1) 
                        pure $
                          pand'List
                            [ pany # hasThread # txIns 
                            , pfromData (psndBuiltin # refPair) #== 1
                            , pfromData (psndBuiltin # userPair) #== 1
                            , refLabel #== label100
                            , userLabel #== label222
                            , userTkName #== refTkName
                            , pif (launchConfigF.marketplaceFee #== mempty) 
                                  (pconstant True)
                                  (pany # (paysAtleastValueToAddress 
                                            # launchConfigF.marketplaceFee 
                                            # launchConfigF.marketplaceAddress)
                                            # txInfoFields.outputs)
                            , pif (launchConfigF.projectFee #== mempty) 
                                  (pconstant True)
                                  (pany # (paysAtleastValueToAddress 
                                            # launchConfigF.projectFee 
                                            # launchConfigF.projectAddress)
                                            # txInfoFields.outputs)
                            ]
                      mintRoyalty = unTermCont $ do
                        royaltyPair <- pletC (phead # tkPairs)
                        PPair royaltyLabel royaltyTkName <- pmatchC (pbreakTokenName (pfromData $ pfstBuiltin # royaltyPair))
                        pure $
                          pand'List
                            [ isUtxoSpent
                            , pfromData (psndBuiltin # royaltyPair) #== 1
                            , royaltyLabel #== label500
                            , royaltyTkName #== pto royaltyTN
                            ]
                   in pcond
                        [ (numMinted #== 2, mintNFTCheck)
                        , (numMinted #== 1, mintRoyalty)
                        ]
                        perror
                PBurnNFT _ ->
                  let burnNFTChecks = unTermCont $ do
                        refPair <- pletC (phead # tkPairs)
                        userPair <- pletC (ptryIndex 1 tkPairs)
                        PPair refLabel refTkName <- pmatchC (pbreakTokenName (pfromData $ pfstBuiltin # refPair))
                        PPair userLabel userTkName <- pmatchC (pbreakTokenName (pfromData $ pfstBuiltin # userPair))
                        pure $
                          pand'List
                            [ pfromData (psndBuiltin # refPair) #== -1
                            , pfromData (psndBuiltin # userPair) #== -1
                            , refTkName #== userTkName
                            , refLabel #== label100
                            , userLabel #== label222
                            ]
                      burnRoyaltyChecks = unTermCont $ do
                        royaltyPair <- pletC (phead # tkPairs)
                        let royaltyLabel = psliceLabel $ pfromData (pfstBuiltin # royaltyPair)
                        pure $
                          pand'List
                            [ pfromData (psndBuiltin # royaltyPair) #== -1
                            , royaltyLabel #== label500
                            ]
                      burnAllChecks = unTermCont $ do
                        refPair <- pletC (phead # tkPairs)
                        userPair <- pletC (ptryIndex 1 tkPairs)
                        royaltyPair <- pletC (ptryIndex 2 tkPairs)
                        PPair refLabel refTkName <- pmatchC (pbreakTokenName (pfromData $ pfstBuiltin # refPair))
                        PPair userLabel userTkName <- pmatchC (pbreakTokenName (pfromData $ pfstBuiltin # userPair))
                        let royaltyLabel = psliceLabel $ pfromData (pfstBuiltin # royaltyPair)
                        pure $
                          pand'List
                            [ isUtxoSpent
                            , pfromData (psndBuiltin # refPair) #== -1
                            , pfromData (psndBuiltin # userPair) #== -1
                            , pfromData (psndBuiltin # royaltyPair) #== -1
                            , refLabel #== label100
                            , userLabel #== label222
                            , royaltyLabel #== label500
                            , userTkName #== refTkName
                            ]
                   in pcond
                        [ (numMinted #== 1, burnRoyaltyChecks)
                        , (numMinted #== 2, burnNFTChecks)
                        , (numMinted #== 3, burnAllChecks)
                        ]
                        perror
            )
        )
        (pconstant ())
        perror

pmintNFTPolicyW :: Term s (PTxOutRef :--> PMintingPolicy)
pmintNFTPolicyW = phoistAcyclic $
  plam $ \oref redm ctx -> unTermCont $ do
    (redeemer, _) <- ptryFromC @PNFTMintAction redm
    pure $ popaque $ pvalidateNFTMint # oref # redeemer # ctx

pmetadataControlEvolveNFT :: Term s (PCurrencySymbol :--> PMetadataEvolveDatum :--> PEvolveAct :--> PScriptContext :--> PUnit)
pmetadataControlEvolveNFT = phoistAcyclic $
  plam $ \ownershipCS dat redm ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
    infoF <- pletFieldsC @'["inputs", "outputs", "signatories"] ctxF.txInfo
    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
    let ownInput = ptryOwnInput # infoF.inputs # ownRef
    ownInputF <- pletFieldsC @'["address", "value", "datum"] ownInput
    ownValueNoAda <- pletC $ pnoAdaValue # ownInputF.value
    PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC (pfield @"credential" # ownInputF.address)
    PTriple refCS refTN _amt <- pmatchC $ ponlyAsset # ownValueNoAda

    pure $
      pif
        ( pmatch
            redm
            ( \case
                PEvolveNFT _ -> unTermCont $ do
                  datF <- pletFieldsC @'["metadata", "version", "extra"] dat
                  evolutionInfoF <- pletFieldsC @'["stage", "stages", "price", "recipient", "isPaid"] datF.extra
                  let ownOutput = ptryOwnOutput # infoF.outputs # ownValHash
                  ownOutputF <- pletFieldsC @'["value", "datum"] ownOutput
                  (POutputDatum ownOutputDatum) <- pmatchC ownOutputF.datum
                  let newEvolveDatum = pfromPDatum @PMetadataEvolveDatum # (pfield @"outputDatum" # ownOutputDatum)
                  newEvolveDatumF <- pletFieldsC @["metadata", "version", "extra"] newEvolveDatum
                  newEvolveInfoF <- pletFieldsC @'["stage", "stages", "price", "recipient", "isPaid"] newEvolveDatumF.extra
                  pure $
                    pand'List
                      [ pfromData evolutionInfoF.stage #< pfromData evolutionInfoF.stages
                      , evolutionInfoF.price #== newEvolveInfoF.price
                      , evolutionInfoF.stage #== newEvolveInfoF.stage
                      , evolutionInfoF.stages #== newEvolveInfoF.stages
                      , evolutionInfoF.recipient #== newEvolveInfoF.recipient
                      , pfromData newEvolveInfoF.isPaid #== pconstant False
                      , ownValueNoAda #== pnoAdaValue # ownOutputF.value
                      , pany @PBuiltinList
                          # plam (\txIn -> pvalueOf # (pfield @"value" # (pfield @"resolved" # txIn)) # ownershipCS # ownershipTN #== 1)
                          # infoF.inputs
                      ]
                PFundEvolution _ -> unTermCont $ do
                  datF <- pletFieldsC @'["metadata", "version", "extra"] dat
                  evolutionInfoF <- pletFieldsC @'["stage", "stages", "price", "recipient", "isPaid"] datF.extra
                  let ownOutput = ptryOwnOutput # infoF.outputs # ownValHash
                      paymentOutput = ptryOutputToAddress # infoF.outputs # evolutionInfoF.recipient
                  ptraceC (pshow paymentOutput)
                  ownOutputF <- pletFieldsC @'["value", "datum"] ownOutput
                  (POutputDatum ownOutputDatum) <- pmatchC ownOutputF.datum
                  let newEvolveDatum = pfromPDatum @PMetadataEvolveDatum # (pfield @"outputDatum" # ownOutputDatum)
                  newEvolveDatumF <- pletFieldsC @'["metadata", "version", "extra"] newEvolveDatum
                  newEvolveInfoF <- pletFieldsC @'["stage", "stages", "price", "recipient", "isPaid"] newEvolveDatumF.extra
                  pure $
                    pand'List
                      [ pfromData evolutionInfoF.stage #< pfromData evolutionInfoF.stages
                      , pfromData evolutionInfoF.price #<= pfromData (pfield @"value" # paymentOutput)
                      , pfromData evolutionInfoF.stage + 1 #== pfromData newEvolveInfoF.stage
                      , evolutionInfoF.stages #== newEvolveInfoF.stages
                      , evolutionInfoF.price #== newEvolveInfoF.price
                      , evolutionInfoF.recipient #== newEvolveInfoF.recipient
                      , pfromData newEvolveInfoF.isPaid #== pconstant True
                      , ownValueNoAda #== pnoAdaValue # ownOutputF.value
                      , datF.metadata #== newEvolveDatumF.metadata
                      ]
                PReclaim _ -> pvalueOf # ownValueNoAda # refCS # refTN #== -1
            )
        )
        (pconstant ())
        perror

pmetadataControlEvolveNFTW :: Term s (PAsData PCurrencySymbol :--> PValidator)
pmetadataControlEvolveNFTW = phoistAcyclic $
  plam $ \ownershipCS dat redm ctx -> unTermCont $ do
    (datum, _) <- ptryFromC @PMetadataEvolveDatum dat
    (redeemer, _) <- ptryFromC @PEvolveAct redm
    pure $ popaque $ pmetadataControlEvolveNFT # pfromData ownershipCS # datum # redeemer # ctx

pmetadataControlFailNFTW :: Term s (PAsData PCurrencySymbol :--> PValidator)
pmetadataControlFailNFTW = phoistAcyclic $
  plam $ \cs _dat _redm _ctx -> popaque $ pif (cs #== cs) perror perror

nftMintPolicyH :: Config -> TxOutRef -> MintingPolicy
nftMintPolicyH cfg oref =
  mkMintingPolicy cfg (pmintNFTPolicyW # pconstant oref)

nftMintCurrencySymbolH :: Config -> TxOutRef -> CurrencySymbol
nftMintCurrencySymbolH cfg oref = mintingPolicySymbol $ nftMintPolicyH cfg oref
