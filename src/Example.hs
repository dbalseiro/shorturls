{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Example (runApp, app) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S

import Control.Monad.IO.Class (liftIO)
import Control.Monad (replicateM)
import System.Random (randomRIO)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as T
import qualified Database.Redis as Hedis

port :: Int
port = 8888

lifetime :: Integer
lifetime = 604800 -- 1 week in seconds

newRandomID :: IO B8.ByteString
newRandomID = B8.pack <$> replicateM 10 (randomRIO ('a', 'z'))

storeUrl
  :: Hedis.Connection
  -> B8.ByteString
  -> B8.ByteString
  -> S.ActionM (Either Hedis.Reply Hedis.Status)
storeUrl connpool randomID longUrl =
  liftIO $ Hedis.runRedis connpool $ Hedis.setex randomID lifetime longUrl

app' :: Hedis.Connection -> T.Text -> S.ScottyM ()
app' connpool defaultUrl = do
  S.get "/healthcheck" $ S.text "hello"

  S.post "/new-url" $ do
    longURL <- S.param "long-url"
    randomID <- liftIO newRandomID
    storeUrl connpool randomID (T.encodeUtf8 longURL)
    S.text $ defaultUrl <> (T.fromStrict . T.decodeUtf8 $ randomID)

  S.get "/:random_id" $ do
    randomID <- S.param "random_id"
    emurl <- liftIO $ Hedis.runRedis connpool (Hedis.get randomID)
    case emurl of
      Left _ -> S.text "500 - Whatever"
      Right Nothing -> S.text "404 - Not Found"
      Right (Just url) -> S.redirect . T.fromStrict . T.decodeUtf8 $ url

runAppWith :: (S.ScottyM () -> IO b) -> IO b
runAppWith f = do
  let localUrl = "http://localhost:" <> show port <> "/"
  connpool <- Hedis.checkedConnect Hedis.defaultConnectInfo
  defaultUrl <- T.pack . fromMaybe localUrl <$> lookupEnv "DEFAULT_URL"
  f $ app' connpool defaultUrl

app :: IO Application
app = runAppWith S.scottyApp

runApp :: IO ()
runApp = runAppWith (S.scotty port)
