{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Confirmator (confirmarTransacao, cancelarTransacao) where

import Data.Aeson (Value, encode)

import qualified Data.Text as T
import Control.Monad (void)

import Text.URI (mkURI, URI)
import Network.HTTP.Req
  ( req
  , runReq
  , defaultHttpConfig
  , POST(..)
  , ReqBodyBs(..)
  , ReqBodyLbs(..)
  , ignoreResponse
  , header
  , Url
  , Scheme(Http)
  , Option
  , useURI
  , port
  , http
  , (/:)
  )


webhookUrl :: T.Text -> IO (Url 'Http)
webhookUrl path = do
  -- Build the URL manually with req's helpers, forcing port 5001
  let base = http "localhost"
      segments = T.splitOn "/" path
      url = foldl (/:) base segments
  -- Attach port 5001 via `port` option
  -- Since `Url` doesn't hold port, we return the url and use `port` option in req calls
  return url

postJson :: T.Text -> Value -> IO ()
postJson path payload = do
  url <- webhookUrl path
  print $ "Sending to: " <> show url
  void $ runReq defaultHttpConfig $ req
    POST
    url
    (ReqBodyLbs $ encode payload)
    ignoreResponse
    (header "Content-Type" "application/json" <> port 5001)


confirmarTransacao :: Value -> IO ()
confirmarTransacao = postJson "confirmar"

cancelarTransacao :: Value -> IO ()
cancelarTransacao = postJson "cancelar"
