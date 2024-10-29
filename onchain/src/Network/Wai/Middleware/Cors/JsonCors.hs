module Network.Wai.Middleware.Cors.JsonCors (jsonCors) where

import Network.Wai (Middleware, mapResponseHeaders)
import Network.Wai.Header (replaceHeader)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy,
  cors,
  corsRequestHeaders,
  simpleCorsResourcePolicy,
 )

allowContentType :: CorsResourcePolicy
allowContentType = simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}

allowACAHContentType :: Middleware
allowACAHContentType app req f =
  app req $
    f
      . mapResponseHeaders
        (replaceHeader "Access-Control-Allow-Headers" "Content-Type")

jsonCors :: Middleware
jsonCors = allowACAHContentType . cors (const $ Just allowContentType)
