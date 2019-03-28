{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=))
import           Data.Aeson.Parser        (json)
import           Data.ByteString          (ByteString)
import           Data.Conduit             (connect)
import           Data.Conduit.Attoparsec  (sinkParser)
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, responseLBS)
import           Network.Wai.Conduit      (sourceRequestBody)
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (lookupEnv)

lookupPort :: IO (Maybe Int)
lookupPort = fmap read <$> lookupEnv "PORT"

main :: IO ()
main =
  lookupPort >>= \case
    Just n -> run n app
    _ -> run 9000 app

app :: Application
app req sendResponse =
  handle (sendResponse . invalidJson) $ do
    value <- sourceRequestBody req `connect` sinkParser json
    newValue <- liftIO $ pure value
    sendResponse $
      responseLBS status200 [("Content-Type", "application/json")] $
      encode newValue

invalidJson :: SomeException -> Response
invalidJson ex =
  responseLBS status400 [("Content-Type", "application/json")] $
  encode $ object [("message" .= show ex)]
