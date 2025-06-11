{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Webhook (handleWebhook)

main :: IO ()
main = scotty 5000 $ do
  post "/webhook" handleWebhook