{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Example (runApp, app) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S

app' :: S.ScottyM ()
app' = do
  S.get "/healthcheck" $ S.text "hello"

app :: IO Application
app = S.scottyApp app'

runApp :: IO ()
runApp = S.scotty 8888 app'
