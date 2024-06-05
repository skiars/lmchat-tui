{-# LANGUAGE OverloadedStrings #-}
module Completion (
  Message,
  StreamData (..),
  StreamHandle,
  CompletionUsage,
  message,
  chatCompletion,
  cancelStream,
  showUsage,
) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Lens
import Data.List (singleton)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAuthorization, hContentType)
import Data.Maybe (fromMaybe, isNothing)
import Control.Concurrent
import Control.Monad (when)
import Control.Lens hiding (Choice, (.=), use)
import AppConfig

newtype StreamHandle = StreamHandle (MVar ())

data Message = Message {
  role :: Text,
  content :: Text
} deriving (Generic, Show)

data CompletionResponse = CompletionResponse {
  choices :: [Choice],
  usage :: Maybe CompletionUsage
} deriving (Generic, Show)

newtype Choice = Choice {
  delta :: Value
} deriving (Generic, Show)

data CompletionUsage = CompletionUsage {
  completion_tokens :: Int,
  prompt_tokens :: Int,
  total_tokens :: Int
} deriving (Generic, Eq, Show)

instance ToJSON Message
instance FromJSON CompletionResponse
instance FromJSON Choice
instance FromJSON CompletionUsage

data StreamData = StreamDelta Text
                | StreamUsage CompletionUsage
                | StreamError String
                | StreamComplete
                deriving (Eq, Show)

message :: Text -> Text -> Message
message = Message

showUsage :: CompletionUsage -> String
showUsage c = "completion: " <> show c.completion_tokens
           <> "tok prompt: " <> show c.prompt_tokens
           <> "tok total: " <> show c.total_tokens <> "tok"

mergeObject :: Value -> Value -> Value
mergeObject (Object l) (Object r) = Object $ r <> l
mergeObject _ r = r

chatCompletion :: ConfigApi
  -> IO ([Message] -> (StreamData -> IO ()) -> IO StreamHandle)
chatCompletion cfg = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ (cfg^.cfgApiBaseUrl) <> "/chat/completions"
  return $ \msgs proc -> do
    let body = mergeObject (cfg^.cfgApiParams) $ object [
          "model" .= (cfg^.cfgApiModel),
          "messages" .= msgs,
          "stream" .= True]
        request = initialRequest {
          method = "POST", requestBody = RequestBodyLBS $ encode body,
          requestHeaders = [
            (hAuthorization, "Bearer " <> encodeUtf8 (T.pack (cfg^.cfgApiKey))),
            (hContentType, "application/json"),
            ("Accept", "text/event-stream")
          ],
          responseTimeout = responseTimeoutNone}
    cancel <- newEmptyMVar
    _ <- forkIO $ withResponse request manager $ \response -> do
      let loop = do
            chunk <- brReadSome (responseBody response) 1024
            if BL.null chunk
              then proc StreamComplete
              else do
                let chks = filter (not . BL.null) $ BL.split 0xa chunk
                mapM_ proc $ concatMap processChunk chks
                r <- tryTakeMVar cancel
                when (isNothing r)
                  loop
      loop
    return $ StreamHandle cancel

cancelStream :: StreamHandle -> IO ()
cancelStream (StreamHandle h) = putMVar h ()

processChunk :: BL.ByteString -> [StreamData]
processChunk chunk = do
  let bytes = fromMaybe chunk $ BL.stripPrefix "data: " chunk
      f d = StreamDelta <$> d ^? key "content" . _String
  case eitherDecode bytes of
    Left err -> [StreamError $ "Error decoding JSON: " <> err]
    Right (CompletionResponse chs use) ->
      (fromMaybe (StreamDelta "") . f . delta <$> chs)
        <> maybe [] (singleton . StreamUsage) use
