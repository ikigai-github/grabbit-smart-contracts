module Vulcan.Common.Constants (
  finSetValidatorFileName,
  finSetSepScriptFileName,
  privateAuctionScriptFileName,
  publicAuctionScriptFileName,
  finSetScriptFileName,
  pubStateTokenScriptFileName,
  privStateTokenScriptFileName,
  marketMPFileName,
  marketValidatorFileName,
  scriptOutDir,
  marketOutDir,
  auxiliaryOutDir,
  scriptTraceOutDir,
  maxAmountOfAssetsInLot,
) where

import Data.Kind (Type)

-- | This restriction is to prevent Token Dust attack
maxAmountOfAssetsInLot :: forall (n :: Type). Num n => n
maxAmountOfAssetsInLot = 50

-- | Project root relative path to unapplied compiled script storage directory.
scriptTraceOutDir :: FilePath
scriptTraceOutDir = "compiled/withTrace"

-- | Project root relative path to unapplied compiled script storage directory.
marketOutDir :: FilePath
marketOutDir = "compiled/market"

auxiliaryOutDir :: FilePath
auxiliaryOutDir = "compiled/auxiliary"

-- | Project root relative path to unapplied compiled script storage directory.
scriptOutDir :: FilePath
scriptOutDir = "compiled/noTrace"

-- | File name of the compiled Market MP
marketMPFileName :: FilePath
marketMPFileName = "marketMP.plutus"

-- | File name of the compiled Market Validator
marketValidatorFileName :: FilePath
marketValidatorFileName = "marketValidator.plutus"

-- | File name of the compiled Finite Set Token Script with Unaplied TxOutRef
finSetScriptFileName :: FilePath
finSetScriptFileName = "finSetScript.plutus"

-- | File name of the compiled FinSet Validator
finSetValidatorFileName :: FilePath
finSetValidatorFileName = "finSetValidator.plutus"

-- | File name of the compiled Finite Set Separator Token Script with Unaplied TxOutRef
finSetSepScriptFileName :: FilePath
finSetSepScriptFileName = "finSetSepScript.plutus"

-- | File name of the compiled Private Auction State Token Script with Unaplied Auction Terms, FinSet CS and TxOutRef
pubStateTokenScriptFileName :: FilePath
pubStateTokenScriptFileName = "pubStateTokenScript.plutus"

-- | File name of the compiled Private Auction State Token Script with Unaplied Auction Terms, FinSet CS and TxOutRef
privStateTokenScriptFileName :: FilePath
privStateTokenScriptFileName = "privStateTokenScript.plutus"

-- | File name of the compiled Private Auction Validator.
privateAuctionScriptFileName :: FilePath
privateAuctionScriptFileName = "privateAuctionValidator.plutus"

-- | File name of the compiled Public Auction Validator.
publicAuctionScriptFileName :: FilePath
publicAuctionScriptFileName = "publicAuctionValidator.plutus"

singleNFTMintScriptFileName :: FilePath
singleNFTMintScriptFileName = "nftMP.plutus"
