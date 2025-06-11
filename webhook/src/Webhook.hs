{-# LANGUAGE OverloadedStrings #-}

module Webhook (handleWebhook) where

import Web.Scotty
import Data.Aeson (eitherDecode)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Validator (validatePayload, validateToken)
import Confirmator (confirmarTransacao, cancelarTransacao)
import Control.Monad.IO.Class (liftIO)

handleWebhook :: ActionM ()
handleWebhook = do
  token <- header "X-Webhook-Token"
  body <- body
  case eitherDecode body of
    Left _ -> do
      status badRequest400
      text "Invalid JSON payload"
    Right jsonPayload -> do
      case token of
        Just t | validateToken t -> process jsonPayload
        _ -> do
          status unauthorized401
          text "Unauthorized"
  where
    process payload = do
      result <- liftIO $ validatePayload payload
      case result of
        Right _ -> do
          _ <- liftIO $ confirmarTransacao payload
          status ok200
          text "Confirmed"
        Left _ -> do
          _ <- liftIO $ cancelarTransacao payload
          status badRequest400
          text "Cancelled"