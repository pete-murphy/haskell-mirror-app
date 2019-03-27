{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=))
import           Data.Aeson.Parser        (json)
import           Data.ByteString          (ByteString)
import           Data.Conduit             (($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, responseLBS)
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (getEnv)

getPort :: IO Int
getPort = read <$> getEnv "PORT"

main :: IO ()
main = do
  port <- getPort
  run port app

app :: Application
app req sendResponse =
  handle (sendResponse . invalidJson) $ do
    value <- sourceRequestBody req $$ sinkParser json
    newValue <- liftIO $ pure value
    sendResponse $
      responseLBS status200 [("Content-Type", "application/json")] $
      encode newValue

invalidJson :: SomeException -> Response
invalidJson ex =
  responseLBS status400 [("Content-Type", "application/json")] $
  encode $ object [("message" .= show ex)]

-- Application-specific logic would go here.
modValue :: Value -> IO Value
modValue = return
