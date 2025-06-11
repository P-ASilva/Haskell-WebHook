{-# LANGUAGE OverloadedStrings #-}

module Validator (
  validatePayload,
  validateToken,
  validatePayloadWithDupCheck,
  initDuplicateChecker
) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Scientific (Scientific)
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Data.IORef
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

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

-- Estado global para IDs duplicados (enquanto n tem db)
{-# NOINLINE duplicateSetRef #-}
duplicateSetRef :: IORef (Set.Set Text)
duplicateSetRef = unsafePerformIO $ newIORef Set.empty

-- Inicializa o estado, opcional para reiniciar o conjunto
initDuplicateChecker :: IO ()
initDuplicateChecker = writeIORef duplicateSetRef Set.empty

isDuplicate :: Text -> IO Bool
isDuplicate tid = do
  set <- readIORef duplicateSetRef
  return $ Set.member tid set

registerTransaction :: Text -> IO ()
registerTransaction tid = do
  modifyIORef' duplicateSetRef (Set.insert tid)

validatePayload :: Value -> IO (Either String Payload)
validatePayload val = do
  putStrLn $ "Raw payload JSON: " ++ BSL8.unpack (encode val)
  case fromJSON val of
    Error e -> return $ Left e
    Success p ->
      if amount p > 0 then return $ Right p
      else return $ Left "Invalid amount"

-- Validação + checagem de duplicado
validatePayloadWithDupCheck :: Value -> IO (Either String Payload)
validatePayloadWithDupCheck val = do
  putStrLn $ "Raw payload JSON: " ++ BSL8.unpack (encode val)
  case fromJSON val of
    Error e -> return $ Left e
    Success p -> do
      if amount p <= 0
        then return $ Left "Invalid amount"
        else do
          dup <- isDuplicate (transaction_id p)
          if dup
            then return $ Left "Duplicate transaction"
            else do
              registerTransaction (transaction_id p)
              return $ Right p