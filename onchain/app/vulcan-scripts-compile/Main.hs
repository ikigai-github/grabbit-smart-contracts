{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception (catch, throwIO)
import Control.Monad (unless)
import GHC.Stack (HasCallStack)

import System.Directory (createDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Error (isAlreadyExistsError)

import Plutarch.Api.V2 (
  PMintingPolicy,
  PValidator,
  mintingPolicySymbol,
  mkMintingPolicy,
  mkValidator,
  validatorHash,
 )
import Ply (readTypedScript)
import Ply qualified
import Ply.Plutarch (writeTypedScript)
import Vulcan.Common.Types.Instances (
  AuctionScripts (..),
 )

import Ply.Core.Serialize.Script (
  serializeScriptCbor,
  serializeScriptCborHex,
 )

import Data.Foldable (fold)
import Scripts.V2.Serialize (scriptSize)
import System.Environment (getArgs)
import Vulcan.Common.Constants qualified as Constant
import Vulcan.Common.Types.FinSet (setNodePrefix)
import Vulcan.Common.Types.Instances qualified as Vulcan
import Vulcan.Initialisation.Parameters (marketORef)
import Vulcan.Onchain.Auction.StateMP.Private (pPrivStateMP)
import Vulcan.Onchain.Auction.StateMP.Public (pPubStateMP)
import Vulcan.Onchain.Auction.Validator.Private (pPrivateAuctionValidator)
import Vulcan.Onchain.Auction.Validator.Public (pPublicAuctionValidator)
import Vulcan.Onchain.Collections.BulkMint (pbulkMintPolicyW)
import Vulcan.Onchain.Collections.OnchainFTMint (pmetadataControlFTW, pmintFTPolicyW)
import Vulcan.Onchain.Collections.SequentialMint (pseqStateMintingPolicy, pseqValidatorW, psequentialNFTMintW)
import Vulcan.Onchain.FinSet.MP.Separator (pSepNodeMP)
import Vulcan.Onchain.FinSet.MP.Standard (pNodeMP)
import Vulcan.Onchain.FinSet.Validator (pFinSetValidator)
import Vulcan.Onchain.Market.MP (pMarketMP)
import Vulcan.Onchain.Market.Validator (pMarketValidator)
import Vulcan.Onchain.Metadata.OnchainNFTMint (pmetadataControlEvolveNFTW, pmintNFTPolicyW)
import Vulcan.Utils (toPlutarchConfig)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (
  catchE,
  runExceptT,
 )
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.TH (
  Options (..),
  defaultOptions,
  deriveJSON,
 )
import Data.ByteString.Lazy (
  toStrict,
 )
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing), compile)
import PlutusLedgerApi.V2 (Script)
import Vulcan.Onchain.Collections.DirectTransfer (pdirectOfferValidatorW)
import Prelude hiding (writeFile)

data PlutusScriptJSON = PlutusScriptJSON
  { plsj_type :: String
  , plsj_description :: String
  , plsj_cborHex :: String
  }
  deriving stock (Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = \f -> fromMaybe f (stripPrefix "plsj_" f)} ''PlutusScriptJSON)

toV2ScriptJson :: Script -> PlutusScriptJSON
toV2ScriptJson script =
  PlutusScriptJSON
    { plsj_type = "PlutusScriptV2"
    , plsj_description = ""
    , plsj_cborHex = T.unpack $ serializeScriptCborHex script
    }

writeJSON :: ToJSON a => FilePath -> a -> IO (Either IOError ())
writeJSON path a = runExceptT $ flip catchE id $ lift (writeFile path content)
  where
    content = decodeUtf8 $ toStrict $ encodePretty a

writeScript :: Config -> String -> FilePath -> ClosedTerm a -> IO ()
writeScript cfg name path t = do
  putStrLn name
  let script = compileD cfg t
      scriptSerial = toV2ScriptJson script
  result <- writeJSON path scriptSerial
  case result of
    Left _ -> error "writeFileTextEnvelope failed"
    Right () -> pure ()

compileD :: Config -> ClosedTerm a -> Script
compileD cfg t =
  let tmode :: TracingMode
      tmode = tracingMode cfg
   in either (error . T.unpack) id $ compile (Config {tracingMode = tmode}) t

refreshDirectory :: FilePath -> IO ()
refreshDirectory dirPath = do
  removeDirectoryRecursive dirPath `catch` (\e -> isAlreadyExistsError e `unless` throwIO e)
  createDirectory dirPath

compileMarket :: IO ()
compileMarket = do
  let cfg = Vulcan.Config {tracingMode = Vulcan.DoTracing Vulcan.Conscise}
      pcfg = toPlutarchConfig cfg
      dir = Constant.marketOutDir

  -- Market:
  -- Market Minting Policy
  putStrLn "Market Minting Policy Script"
  writeTypedScript
    pcfg
    "Market Token Minting Policy"
    (dir </> Constant.marketMPFileName)
    $ pMarketMP cfg marketORef

  marketMPScript <- readTypedScript $ dir </> Constant.marketMPFileName
  let marketCS = mintingPolicySymbol $ Ply.toMintingPolicy marketMPScript

  putStrLn "Market Validator Script"
  -- Market Validator
  writeTypedScript
    pcfg
    "Market Validator"
    (dir </> Constant.marketValidatorFileName)
    $ pMarketValidator cfg marketCS

compileAux :: IO ()
compileAux = do
  let cfg = Vulcan.Config {tracingMode = Vulcan.DoTracing Vulcan.Conscise}
      pcfg = toPlutarchConfig cfg
      dir = Constant.auxiliaryOutDir

  writeScript
    pcfg
    "NFT - CIP 68 Metadata Minting Policy"
    (dir </> "onchainNFT.plutus")
    pmintNFTPolicyW

  writeScript
    pcfg
    "NFT - Onchain Metadata Control Validator"
    (dir </> "metadataControlEvolveNFT.plutus")
    pmetadataControlEvolveNFTW

  writeScript
    pcfg
    "Fungible Token - CIP 68 Metadata Minting Policy"
    (dir </> "onchainFT.plutus")
    pmintFTPolicyW

  writeScript
    pcfg
    "Fungible Token - Onchain Metadata Control Validator"
    (dir </> "metadataControlFT.plutus")
    pmetadataControlFTW

  writeScript
    pcfg
    "Bulk Mint Minting Policy"
    (dir </> "bulkMintingPolicy.plutus")
    pbulkMintPolicyW

  writeScript
    pcfg
    "Sequential State Minting Policy"
    (dir </> "seqStateMintingPolicy.plutus")
    pseqStateMintingPolicy

  writeScript
    pcfg
    "Sequence Validator"
    (dir </> "sequenceValidator.plutus")
    pseqValidatorW

  writeScript
    pcfg
    "Sequential NFT Minting Policy"
    (dir </> "sequentialNFTMintingPolicy.plutus")
    psequentialNFTMintW

  writeScript
    pcfg
    "Direct Offer Validator"
    (dir </> "directOffer.plutus")
    pdirectOfferValidatorW

compileScripts :: FilePath -> FilePath -> Vulcan.Config -> IO ()
compileScripts pathPrefix ((pathPrefix </>) -> dir) cfg = do
  let pcfg = toPlutarchConfig cfg
  refreshDirectory dir
  marketMPScript <- readTypedScript $ pathPrefix </> Constant.marketOutDir </> Constant.marketMPFileName
  let marketCS = mintingPolicySymbol $ Ply.toMintingPolicy marketMPScript
  print marketCS
  ------------------------------
  -- FinSet:
  -- FinSet Validator
  putStrLn "FinSet Validator Script"
  writeTypedScript
    pcfg
    "FinSet Validator"
    (dir </> Constant.finSetValidatorFileName)
    $ pFinSetValidator cfg setNodePrefix
  finSetValScript <- readTypedScript $ dir </> Constant.finSetValidatorFileName
  let finSetValHash = validatorHash $ Ply.toValidator finSetValScript
  -- FinSet Minting Policy
  writeTypedScript
    pcfg
    "Finset Minting Policy with Unapplied TxOutRef"
    (dir </> Constant.finSetScriptFileName)
    $ pNodeMP cfg marketCS finSetValHash

  -- FinSet Minting Policy With Separators
  writeTypedScript
    pcfg
    "Finset Separator Minting Policy with Unapplied TxOutRef and Separator cfg"
    (dir </> Constant.finSetSepScriptFileName)
    $ pSepNodeMP cfg marketCS finSetValHash

  ------------------------------
  -- Private Auctions:
  -- Private auction Validator
  writeTypedScript
    pcfg
    "Private Auction Validator"
    (dir </> Constant.privateAuctionScriptFileName)
    $ pPrivateAuctionValidator cfg marketCS

  privAuctionScript <- readTypedScript $ dir </> Constant.privateAuctionScriptFileName
  let privValHash = validatorHash $ Ply.toValidator privAuctionScript
  -- Private State Token Minting Policy
  writeScript
    pcfg
    "State Token Minting Policy with Unapplied AuctionTerms, FinSet CurrencySymbol and TxOutRef for Private Auction"
    (dir </> Constant.privStateTokenScriptFileName)
    $ pPrivStateMP cfg privValHash marketCS
  -----------------------------
  -- Public Auctions:
  -- Public auction Validator
  writeTypedScript
    pcfg
    "Public Auction Validator"
    (dir </> Constant.publicAuctionScriptFileName)
    $ pPublicAuctionValidator cfg marketCS
  -- Public State Token Minting Policy
  pubAuctionScript <- readTypedScript $ dir </> Constant.publicAuctionScriptFileName
  let pubValHash = validatorHash $ Ply.toValidator pubAuctionScript

  putStrLn "Pub Validator Hash:"
  print pubValHash
  writeScript
    pcfg
    "State Token Minting Policy with Unapplied AuctionTerms, FinSet CurrencySymbol and TxOutRef for Public Auction"
    (dir </> Constant.pubStateTokenScriptFileName)
    $ pPubStateMP cfg pubValHash marketCS

  writeScript
    pcfg
    "Bulk Mint Minting Policy"
    (dir </> "bulkMintingPolicy.plutus")
    pbulkMintPolicyW

  writeScript
    pcfg
    "Sequential State Minting Policy"
    (dir </> "seqStateMintingPolicy.plutus")
    pseqStateMintingPolicy

  writeScript
    pcfg
    "Sequence Validator"
    (dir </> "sequenceValidator.plutus")
    pseqValidatorW

  writeScript
    pcfg
    "Sequential NFT Minting Policy"
    (dir </> "sequentialNFTMintingPolicy.plutus")
    psequentialNFTMintW

  writeScript
    pcfg
    "Direct Offer Validator"
    (dir </> "directOffer.plutus")
    pdirectOfferValidatorW

main :: IO ()
main = do
  compileMarket
  compileAux
  pathPrefix <- fold <$> getArgs
  putStrLn "Compiling scripts without tracing..."
  compileScripts
    pathPrefix
    Constant.scriptOutDir
    Vulcan.Config {tracingMode = Vulcan.NoTracing}
  putStrLn "Compiling scripts with tracing..."
  compileScripts
    pathPrefix
    Constant.scriptTraceOutDir
    Vulcan.Config {tracingMode = Vulcan.DoTracing Vulcan.Conscise}

-- getScriptSizes $ pathPrefix </> Constant.scriptOutDir

getScriptSizes :: FilePath -> IO ()
getScriptSizes dir = do
  let marketdir = Constant.marketOutDir
  finSetValScript <- readTypedScript $ dir </> Constant.finSetValidatorFileName
  finSetMPScript <- readTypedScript $ dir </> Constant.finSetScriptFileName
  finSetSepMPScript <- readTypedScript $ dir </> Constant.finSetSepScriptFileName
  pubAuctionScript <- readTypedScript $ dir </> Constant.publicAuctionScriptFileName
  privAuctionScript <- readTypedScript $ dir </> Constant.privateAuctionScriptFileName
  pubStateScript <- readTypedScript $ dir </> Constant.pubStateTokenScriptFileName
  privStateScript <- readTypedScript $ dir </> Constant.privStateTokenScriptFileName
  marketMPScript :: Ply.TypedScript 'Ply.MintingPolicyRole '[] <- readTypedScript $ marketdir </> Constant.marketMPFileName
  marketScript :: Ply.TypedScript 'Ply.ValidatorRole '[] <- readTypedScript $ marketdir </> Constant.marketValidatorFileName
  let privAuction = MkAuctionScripts privAuctionScript finSetValScript (Vulcan.StandardFinSet finSetMPScript) privStateScript
      pubAuction = MkAuctionScripts pubAuctionScript finSetValScript (Vulcan.SeparatorFinSet finSetSepMPScript) pubStateScript
      marketMP = Ply.toScript marketMPScript
      marketValidator = Ply.toScript marketScript
      showSizeB name a = name <> ": " <> show a <> " bytes \n"
  putStrLn "Private Auction Script Sizes:"
  putStrLn $ sizes privAuction
  putStrLn "Public Auction Script Sizes:"
  putStrLn $ sizes pubAuction
  putStrLn $ showSizeB "Market MP" $ scriptSize marketMP
  putStrLn $ showSizeB "Market Validator" $ scriptSize marketValidator

sizes :: AuctionScripts -> String
sizes scripts =
  showSizeB "FinSet Validator " node
    <> showSizeB "FinSet MP" nodeMP
    <> showSizeB "Auction Validator" auction
    <> showSizeB "State MP" stateMP
  where
    showSizeB name a = name <> ": " <> show a <> " bytes \n"
    node = scriptSize $ Ply.toScript scripts.nodeScript
    nodeMP = scriptSize $ case scripts.nodeMPScript of
      Vulcan.StandardFinSet script -> Ply.toScript script
      Vulcan.SeparatorFinSet script -> Ply.toScript script
    auction = scriptSize $ Ply.toScript scripts.auctionScript
    stateMP = scriptSize $ Ply.toScript scripts.stateMPScript
