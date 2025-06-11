{-# LANGUAGE OverloadedStrings #-}

module Validator (validatePayload, validateToken) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Scientific (Scientific)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- Define o tipo esperado para o payload
data Payload = Payload
  { event :: Text
  , transaction_id :: Text
  , amount :: Scientific
  , currency :: Text
  , timestamp :: Text
  } deriving Show

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \v -> Payload
    <$> v .: "event"
    <*> v .: "transaction_id"
    <*> v .: "amount"
    <*> v .: "currency"
    <*> v .: "timestamp"

validateToken :: TL.Text -> Bool
validateToken t = t == "meu-token-secreto"

validatePayload :: Value -> IO (Either String Payload)
validatePayload val = do
  -- Print raw JSON payload as String
  putStrLn $ "Raw payload JSON: " ++ BSL8.unpack (encode val)
  
  case fromJSON val of
    Error e -> return $ Left e
    Success p ->
      if amount p > 0 then return $ Right p
      else return $ Left "Invalid amount"
