{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types (module Types) where

import Control.Lens hiding (zoom)
import Brick.BChan (BChan)
import Data.Text (Text)
import WEditor.LineWrap
import WEditorBrick.WrappingEditor
import Completion
import Brick.Widgets.Edit (Editor, editor)
import Brick.Widgets.List (list, GenericList)
import qualified Data.Vector as Vec
import AppConfig

data Name = MainView
          | UserEditor
          | PathEditor
          | FloatEditor
          | ChatList
          | FileList
          | PreviewList
          | SpcMenu
          deriving (Eq, Ord, Show)

data Mode = NormalMode
          | SEditMode
          | MEditMode
          | SelChatMode
          | SpcMenuMode
          | HelpMenuMode
          | SavePickerMode
          | LoadPickerMode
          | SysPromptMode
          | SelChatEditMode
          deriving (Eq, Ord, Show)

data ChatRole = SystemRole
              | UserRole
              | AssistantRole
              deriving (Show, Eq, Enum)

data ChatMessage = ChatMessage {
  _cmRole :: ChatRole,
  _cmContent :: Text
} deriving (Show)
makeLenses ''ChatMessage

data St = St {
  _stMode :: Mode,
  _stStream :: Maybe StreamHandle,
  _stStatusInfo :: String,
  _stChats :: [ChatMessage],
  _stChatSelect :: Maybe Int,
  _stPreviewChat :: [ChatMessage],
  _stUserEditor :: WrappingEditor Name,
  _stFloatEditor :: WrappingEditor Name,
  _stPathEditor :: Editor FilePath Name,
  _stFileList :: GenericList Name Vec.Vector FilePath ,
  _stSessionFiles :: [FilePath],
  _stEventChan :: BChan StreamData,
  _stChatComplete :: [Message] -> (StreamData -> IO ()) -> IO StreamHandle,
  _stApiConfig :: ConfigApi,
  _stConfig :: Config
}
makeLenses ''St

userEditor :: Text -> WrappingEditor Name
userEditor = newEditor breakExact UserEditor

floatEditor :: Text -> WrappingEditor Name
floatEditor = newEditor breakExact FloatEditor

pathEditor :: FilePath -> Editor FilePath Name
pathEditor = editor PathEditor (Just 1)

fileList :: [FilePath] -> GenericList Name Vec.Vector FilePath
fileList v = list FileList (Vec.fromList v) 1
