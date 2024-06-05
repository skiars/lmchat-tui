{-# LANGUAGE OverloadedStrings #-}
module Utils (module Utils) where

import Types
import AppConfig

import qualified Insc as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Lens
import System.Process
import System.Exit
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.FilePath.Glob
import System.FilePath
import qualified Algorithms.NaturalSort as NS
import Data.List (sortBy)
import qualified Graphics.Vty as V
import Data.Bits
import Data.Either (fromLeft)
import Control.Monad (void)
import System.IO.Temp (withSystemTempFile)

runCmd :: String -> Maybe BS.ByteString -> IO (Either String BS.ByteString)
runCmd cmd inData = do
  let cp = (shell cmd) {
          std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
  withCreateProcess cp $ \hin hout herr hp -> do
    let put h = maybe (return ()) (BS.hPut h) inData >> hClose h
        get = maybe (return "") BS.hGetContents
    maybe (return ()) put hin
    res <- waitForProcess hp
    out <- get hout
    case res of
      ExitSuccess -> return $ Right out
      ExitFailure _ ->
        Left . T.unpack . decodeUtf8 <$> get herr

runCmd' :: String -> Maybe BS.ByteString -> IO ()
runCmd' cmd = void . runCmd cmd

yankData :: Config -> BS.ByteString -> IO String
yankData cfg d = fromLeft "data yanked" <$> cmd (Just d) where
  cmd = maybe f runCmd $ cfg ^. cfgCmds . cfgCmdYank
  f = const $ return $ Left "no yank command"

yankInsc :: Config -> [ChatMessage] -> IO String
yankInsc cfg = yankData cfg . BS.toStrict . Seq.encodeSeq . toIns

pasteText :: Config -> IO (Either String Text)
pasteText cfg = fmap decodeUtf8 <$> cmd where
  cmd = maybe f (`runCmd` Nothing)  $ cfg ^. cfgCmds . cfgCmdPaste
  f = return $ Left "no paste command"

yankText :: Config -> Text -> IO String
yankText cfg = yankData cfg . encodeUtf8

toIns :: [ChatMessage] -> Seq.Seq
toIns = Seq.Seq . fmap f where
  f (ChatMessage role content) = (g role, content)
  g SystemRole = Seq.SystemRole
  g UserRole = Seq.UserRole
  g AssistantRole = Seq.AssistantRole

loadIns :: FilePath -> IO [ChatMessage]
loadIns path = do
  s <- map Seq.stripSeq <$> Seq.readSeq (path <> ".ins")
  let f (role, content) = ChatMessage (g role) content
      g Seq.SystemRole = SystemRole
      g Seq.UserRole = UserRole
      g Seq.AssistantRole = AssistantRole
      h ((Seq.SystemRole, ""):xs) = xs
      h xs = xs
  case s of
    [] -> return []
    x:_ -> return $ f <$> h x.contents

saveIns :: FilePath -> [ChatMessage] -> IO ()
saveIns path chat = do
  let ins = Seq.encodeSeq $ toIns chat
  BSL.writeFile (path <> ".ins") ins

scanSessionFiles :: FilePath -> IO [FilePath]
scanSessionFiles = fmap f . globDir1 "*.ins" where
  f = sortBy NS.compare . fmap (takeBaseName . takeFileName)

color :: Int -> V.Color
color rgb = V.rgbColor r g b where
  r = rgb `shiftR` 16 .&. 0xff
  g = rgb `shiftR` 8  .&. 0xff
  b = rgb             .&. 0xff

trueColor :: Int -> V.Color
trueColor rgb = V.linearColor r g b where
  r = rgb `shiftR` 16 .&. 0xff
  g = rgb `shiftR` 8  .&. 0xff
  b = rgb             .&. 0xff

selectIME :: Config -> Bool -> IO ()
selectIME cfg s = maybe (return ()) (`runCmd'` Nothing) cmd where
  cmd = cfg ^. cfgCmds .
    if s then cfgCmdImeEnable else cfgCmdImeDisable

editTextExt :: Config -> Text -> IO Text
editTextExt cfg t = case cfg ^. cfgCmds . cfgCmdEditor of
  Just cmd -> withSystemTempFile "lmchat-.md" $ \path h -> do
      TIO.hPutStr h t
      hClose h
      _ <- system $ cmd <> " " <> path
      TIO.readFile path
  Nothing -> return t
