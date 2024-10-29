module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors.JsonCors (jsonCors)
import Scripts.V2.Serialize (asCompiled, asCompiledMP)
import Servant (
  Get,
  JSON,
  Post,
  Proxy (Proxy),
  ReqBody,
  Server,
  serve,
  type (:<|>) ((:<|>)),
  type (:>),
 )
import System.FilePath ((</>))

import PlutusLedgerApi.V2 (CurrencySymbol, TxOutRef)
import Ply (readTypedScript)
import Ply qualified

import Vulcan.Common.Constants (
  finSetScriptFileName,
  finSetValidatorFileName,
  marketMPFileName,
  marketOutDir,
  marketValidatorFileName,
  privStateTokenScriptFileName,
  privateAuctionScriptFileName,
  pubStateTokenScriptFileName,
  publicAuctionScriptFileName,
  scriptOutDir,
  scriptTraceOutDir,
 )
import Vulcan.Common.Types.Instances (
  AuctionParams (config, isPrivate),
  AuctionScripts (MkAuctionScripts),
  CompiledInstance,
  CompiledMarketInstance,
  CompiledScript,
  Config (tracingMode),
  FinSetScript (..),
  MarketInstance' (MkMarketInstance, marketMP, marketValidator),
  TracingMode (DoTracing, NoTracing),
 )
import Vulcan.Compile (auctionInstance)
import Vulcan.Onchain.NFT.MP (nftMP)

type Api =
  "compile" :> Get '[JSON] String
    :<|> "auction"
      :> ReqBody '[JSON] AuctionParams
      :> Post '[JSON] CompiledInstance
    :<|> "market"
      :> ReqBody '[JSON] ()
      :> Post '[JSON] CompiledMarketInstance
    :<|> "nft"
      :> ReqBody '[JSON] TxOutRef
      :> Post '[JSON] (CompiledScript CurrencySymbol)

app :: Server Api
app =
  pure "Server"
    :<|> compiledAuction
    :<|> compiledMarket
    :<|> withAction'
      (asCompiledMP . nftMP)

withAction' :: (MonadIO m, Monad m, Aeson.ToJSON b) => (a -> b) -> a -> m b
withAction' f x = do
  liftIO $ do
    putStrLn "Compiling nft instance..."
  pure (f x)

main :: IO ()
main = do
  putStrLn "vulcan-script-instance server running"
  Warp.run 8080
    . jsonCors
    $ serve (Proxy :: Proxy Api) app

instOutDir :: FilePath
instOutDir = "browser/src/services/script-compiler"

compiledMarket :: (MonadIO m, Monad m) => () -> m CompiledMarketInstance
compiledMarket _ = liftIO $ do
  putStrLn "Fetching the market scripts..."
  let dir = marketOutDir
  marketMPScript <- readTypedScript $ dir </> marketMPFileName
  marketValScript <- readTypedScript $ dir </> marketValidatorFileName
  let marketInst =
        MkMarketInstance
          { marketValidator = Ply.toValidator marketValScript
          , marketMP = Ply.toMintingPolicy marketMPScript
          }
      compiled = asCompiled marketInst
  writeScripts "market" compiled
  pure compiled

compiledAuction :: (MonadIO m, Monad m) => AuctionParams -> m CompiledInstance
compiledAuction params = liftIO $ do
  putStrLn "Preparing the auction scripts..."

  let dir = case tracingMode params.config of
        DoTracing _ -> scriptTraceOutDir
        NoTracing -> scriptOutDir
      (stateFileName, auctionFileName)
        | params.isPrivate = (privStateTokenScriptFileName, privateAuctionScriptFileName)
        | otherwise = (pubStateTokenScriptFileName, publicAuctionScriptFileName)
  finSetValScript <- readTypedScript $ dir </> finSetValidatorFileName
  finSetMPScript <- readTypedScript $ dir </> finSetScriptFileName
  stateMPScript <- readTypedScript $ dir </> stateFileName
  auctionValidatorScript <- readTypedScript $ dir </> auctionFileName
  let scripts = MkAuctionScripts auctionValidatorScript finSetValScript (StandardFinSet finSetMPScript) stateMPScript
      compiled = asCompiled $ auctionInstance scripts params
  writeScripts "auction" compiled
  pure compiled

writeScripts :: forall {a}. Aeson.ToJSON a => FilePath -> a -> IO ()
writeScripts dir comp = LBS.writeFile (instOutDir </> dir </> "instances.json") (Aeson.encode comp)
