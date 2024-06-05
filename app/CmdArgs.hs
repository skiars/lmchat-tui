module CmdArgs (
  CmdOpts (..),
  parseArgs
) where

import Data.Data
import System.Console.CmdArgs

data CmdOpts = CmdOpts {
  cmdApi :: Maybe String
} deriving (Show, Data, Typeable)

opts :: CmdOpts
opts = CmdOpts {
  cmdApi = def &= explicit &= name "api" &= help "API provider"
  } &= summary "lmchat v1"


parseArgs :: IO CmdOpts
parseArgs = cmdArgs opts
