{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module AppConfig (module AppConfig) where

import Data.Yaml
import System.Directory
import System.FilePath
import Control.Lens
import Data.Maybe (fromMaybe)

data Config = Config {
  _cfgApis :: [ConfigApi],
  _cfgCmds :: ConfigCmds
} deriving Show

data ConfigCmds = ConfigCmds {
  _cfgCmdEditor :: Maybe String,
  _cfgCmdYank :: Maybe String,
  _cfgCmdPaste :: Maybe String,
  _cfgCmdImeEnable :: Maybe String,
  _cfgCmdImeDisable :: Maybe String
} deriving Show

data ConfigApi = ConfigApi {
  _cfgApiId :: String,
  _cfgApiBaseUrl :: String,
  _cfgApiKey :: String,
  _cfgApiModel :: String,
  _cfgApiParams :: Value
} deriving Show

makeLenses ''Config
makeLenses ''ConfigCmds
makeLenses ''ConfigApi


instance FromJSON ConfigApi where
  parseJSON (Object v) =
    ConfigApi <$> v .: "id"
              <*> v .: "base-url"
              <*> v .: "key"
              <*> v .: "model"
              <*> (fromMaybe Null <$> v .:? "params")
  parseJSON _ = fail "expect a object"

instance FromJSON ConfigCmds where
  parseJSON (Object v) =
    ConfigCmds <$> v .:? "editor"
               <*> v .:? "yank"
               <*> v .:? "paste"
               <*> v .:? "ime-enable"
               <*> v .:? "ime-disable"
  parseJSON _ = fail "expect a object"

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .: "provider"
           <*> v .: "commands"
  parseJSON _ = fail "expect a object"

loadConfig :: IO Config
loadConfig = do
  cfgDir <- getXdgDirectory XdgConfig "lmchat"
  decodeFileThrow  $ cfgDir </> "config.yaml"
