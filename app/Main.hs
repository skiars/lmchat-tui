{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Lens hiding (zoom)
import Brick hiding (textWidth, txtWrap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.Text (Text)
import Data.List (singleton, find)
import qualified Data.Text as T
import qualified Data.Text as Text
import Control.Monad.Extra
import Graphics.Vty (Event(..), Key(..))
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VC
import Data.Maybe (isNothing, fromJust)
import Brick.Widgets.Center (hCenter, centerLayer)
import qualified Brick.Widgets.Edit as BE
import Brick.Widgets.List hiding (reverse)
import Brick.Widgets.Edit (handleEditorEvent)
import qualified Data.Vector as Vec

import Types
import Utils
import WEditorBrick.WrappingEditor
import Completion
import AppConfig
import CmdArgs

main :: IO ()
main = do
    args <- parseArgs
    chan <- newBChan 10
    let buildVty = VC.mkVty $ V.defaultConfig {
      V.configTermWidthMaps= [
        ("xterm-256color", "/home/gwl/.vty/width_table_screen-256color.dat"),
        ("screen-256color", "/home/gwl/.vty/width_table_screen-256color.dat")
      ]}
    initialVty <- buildVty
    config <- loadConfig
    let apiCfg = lookupApi args.cmdApi $ config^.cfgApis
        modelInfo m = (m^.cfgApiId, m^.cfgApiModel)
        modelList = listMoveToElement (modelInfo apiCfg) $
          list ModelList (Vec.fromList $ modelInfo <$> config^.cfgApis) 1
    complete <- chatCompletion apiCfg
    let st = St
          NormalMode Nothing ""
          [] Nothing []
          (userEditor "")
          (floatEditor "")
          (pathEditor "")
          (fileList [])
          modelList
          []
          chan
          complete
          apiCfg
          config
    void $ customMain initialVty buildVty (Just chan) app st

lookupApi :: Maybe String -> [ConfigApi] -> ConfigApi
lookupApi Nothing  s = head s
lookupApi (Just k) s = fromJust $ find ((== k) . view cfgApiId) s

app :: App St StreamData Name
app = App
    { appDraw = drawUI
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = startEvent
    , appAttrMap = theAttrMap
    }

theAttrMap :: St -> AttrMap
theAttrMap st = attrMap V.defAttr as where
  as = a1 <> a2 <> an
  a1 =  singleton $ if isEditMode st
    then (msgEditAttr, V.defAttr `V.withForeColor` V.green)
    else (msgEditAttr, V.defAttr `V.withForeColor` color 0x808080)
  a2 = singleton $ case st^.stStream of
    Just _  -> (streamStatusAttr, V.defAttr `V.withForeColor` color 0xfcbf49)
    Nothing -> (streamStatusAttr, V.defAttr `V.withForeColor` color 0x57cc99)
  an = [
    (userMsgAttr, V.defAttr `V.withForeColor` color 0xffb703),
    (systemMsgAttr, V.defAttr `V.withForeColor` color 0x778da9),
    (leftBubbleAttr, V.defAttr `V.withForeColor` color 0x808080),
    (rightBubbleAttr, V.defAttr `V.withForeColor` color 0x00b4d8),
    (statusBarAttr, V.defAttr `V.withBackColor` V.black),
    (keyNameAttr, V.defAttr `V.withForeColor` color 0x90e0ef),
    (selectAttr, V.defAttr `V.withForeColor` V.white
                           `V.withBackColor` color 0x606060),
    (focusBorderAttr, V.defAttr `V.withForeColor` V.green),
    (minorMsgAttr, V.defAttr `V.withForeColor` color 0x787878),
    (selChatAttr, V.defAttr `V.withForeColor` color 0x00d7af)
    ]

isEditMode :: St -> Bool
isEditMode st = case st^.stMode of
  SEditMode -> True
  MEditMode -> True
  _         -> False

msgEditAttr :: AttrName
msgEditAttr = attrName "msgEditor"

focusBorderAttr :: AttrName
focusBorderAttr = attrName "focusBorder"

userMsgAttr :: AttrName
userMsgAttr = attrName "userMsg"

systemMsgAttr :: AttrName
systemMsgAttr = attrName "systemMsg"

minorMsgAttr :: AttrName
minorMsgAttr = attrName "minorMsg"

selectAttr :: AttrName
selectAttr = attrName "select"

keyNameAttr :: AttrName
keyNameAttr = attrName "keyName"

streamStatusAttr :: AttrName
streamStatusAttr = attrName "streamStatus"

statusBarAttr :: AttrName
statusBarAttr = attrName "statusBar"

selChatAttr :: AttrName
selChatAttr = attrName "selectChat"

resetAttr :: Widget n -> Widget n
resetAttr = modifyDefAttr (const V.defAttr)

drawUI :: St -> [Widget Name]
drawUI st = [
  topLayerUI st,
  vBox [
    padBottom Max $ chatUI st,
    withDefAttr msgEditAttr $ withBorderStyle unicodeRounded $ border $
      resetAttr $ vLimit 4 $ renderEditor True st._stUserEditor,
    statusBar st]
  ]

statusBar :: St -> Widget Name
statusBar st = withDefAttr statusBarAttr $ padLeft (Pad 1) $ padRight (Pad 1) $
  hBox [
    txt (modeName st),
    hCenter $ withAttr minorMsgAttr $ padLeft (Pad 1) $ padRight (Pad 1) $
      str $ nonEmpty " " st._stStatusInfo,
    padRight (Pad 1) $ str (st^. stApiConfig . cfgApiModel),
    withAttr streamStatusAttr $ case st^.stStream of
      Just _  -> txt "⏳ GENERATING"
      Nothing -> txt "🟢 COMPLETED "
  ]

chatUI :: St -> Widget Name
chatUI st = withVScrollBars OnRight $
  scrollable ChatList $ chatView $ zip chat (f <$> [0..]) where
    chat = st^.stChats
    f x = (Just x ==) $ st^.stChatSelect

chatView :: [(ChatMessage, Bool)] -> Widget Name
chatView chat = w where
  w = case chat of
    [] -> withDefAttr systemMsgAttr $
      hCenter $ withBorderStyle unicodeRounded $
        border $ txt "empty chat session, enter your question!"
    xs -> vBox $ f <$> xs
  bubble alg = txtBubble bubbleAttr {bubbleAlign = alg}
  f (ChatMessage r s, a) = sel a $ case r of
    SystemRole    -> (Just systemMsgAttr, bubble BubbleLeft " 🔧 system " s)
    UserRole      -> (Just userMsgAttr, bubble BubbleRight " 👴 you " s)
    AssistantRole -> (Nothing,  bubble BubbleLeft " 🤖 bot " s)
  sel True (_, b) = withAttr selChatAttr b
  sel _    (Just a, b) = withAttr a b
  sel _    (_     , b) = b

chatView' :: [ChatMessage] -> Widget Name
chatView' = chatView . (`zip` repeat False)

topLayerUI :: St -> Widget Name
topLayerUI st = withBorderStyle unicodeRounded $ case st^.stMode of
  SpcMenuMode     -> spcMenuUI st
  HelpMenuMode    -> helpMenuUI st
  LoadPickerMode  -> pickerMenuUI st
  SavePickerMode  -> pickerMenuUI st
  SelChatEditMode -> floatEditUI st
  SysPromptMode   -> floatEditUI st
  ModelSelectMode -> modelSelectUI st
  _               -> emptyWidget

keyMenu :: [(Text, Text)] -> Widget n
keyMenu = padLeft (Pad 1) . padRight (Pad 1) . vBox . fmap f where
  f (k, s) = withAttr keyNameAttr (txt (T.justifyRight 8 ' ' k))
         <+> padLeft (Pad 1) (txt s)

spcMenuUI :: St -> Widget n
spcMenuUI _ = dialog "menu" menu where
  menu = keyMenu [
      ("p", "edit system prompt"),
      ("l", "load chat session"),
      ("s", "save chat session"),
      ("m", "select model and API provider"),
      ("n", "start a new chat session")
    ]

helpMenuUI :: St -> Widget n
helpMenuUI _ = dialog "key binds" menu where
  menu = keyMenu [
      ("q",       "quit lmchat-tui"),
      ("<space>", "open main menu"),
      ("j",       "scroll down in the messages list"),
      ("k",       "scroll up in the messages list"),
      ("<c-u>",   "scroll up half a screen in the message list"),
      ("<c-d>",   "scroll down half a screen in the message list"),
      ("i",       "enter single-line editing mode"),
      ("o",       "enter multi-line editing mode"),
      ("c",       "clear message input editor"),
      ("r",       "regenerate the last message"),
      ("R",       "re-edit the last user message"),
      ("<enter>", "send current user message"),
      ("<c-c>",   "interrupt generation"),
      ("s",       "enter dialogue selection mode"),
      ("p",       "paste text into user message"),
      ("Y",       "yank whole session to clipboard"),
      ("y",       "yank last message to clipboard")
    ]

dialog :: Text -> Widget n -> Widget n
dialog title = centerLayer . borderWithLabel (txt title)

pickerMenuUI :: St -> Widget Name
pickerMenuUI st = centerLayer $ hLimit 100 $ vLimit 25 $ picker <+> chat where
  picker = borderWithLabel title menu
  menu = hLimit 30 $ vBox [edit, str (replicate 100 '─'), li]
  edit = BE.renderEditor (str . unlines) True (st^.stPathEditor)
  li = withVScrollBars OnRight $
        renderList rl True (st^.stFileList)
  rl True e = withAttr selectAttr $ padRight Max $ str e
  rl _    e = str e
  chat = borderWithLabel (txt "preview") $ padRight Max $ padBottom Max $
    scrollable PreviewList $ chatView' $ previewBrief st
  title = txt $ case st^.stMode of
    LoadPickerMode -> "load picker"
    SavePickerMode -> "save picker"
    _              -> "picker"

modelSelectUI :: St -> Widget Name
modelSelectUI st = centerLayer $ hLimit 60 $ vLimit 20 picker where
  picker = borderWithLabel (str "select model provider") menu
  menu = withVScrollBars OnRight $
    renderList rl True (st^.stModelList)
  rl True e = withDefAttr selectAttr $ padRight Max $ f e
  rl _    e = f e
  f (p, m) = str m <+> str " - " <+> str p

nonEmpty :: String -> String -> String
nonEmpty a "" = a
nonEmpty _ b  = b

scrollable :: Name -> Widget Name -> Widget Name
scrollable n = clickable n . viewport n Vertical

floatEditUI :: St -> Widget Name
floatEditUI st = centerLayer $ hLimit 80 dlg where
  dlg = withDefAttr focusBorderAttr $ withBorderStyle unicodeRounded $
    borderWithLabel title $ vBox
      [editor, str $ replicate 100 '─', hCenter $ txt "<c-s>: confirm, <c-r>: clear"]
  editor = resetAttr $ vLimit 15 $ renderEditor True (st^.stFloatEditor)
  title = txt $ case st^.stMode of
    SysPromptMode   -> "edit system prompt"
    SelChatEditMode -> "edit chat message"
    _               -> "edit text"

previewBrief :: St -> [ChatMessage]
previewBrief st = over cmContent f <$> st^.stPreviewChat where
  f s = case T.splitAt 80 s of
    (a, "") -> a
    (a, _)  -> a <> "…"

modeName :: St -> Text
modeName st = case st^.stMode of
  SEditMode       -> "S-EDIT"
  MEditMode       -> "M-EDIT"
  SelChatMode     -> "SELECT"
  SelChatEditMode -> "SELECT"
  _               -> "NORMAL"

startEvent :: EventM n s ()
startEvent = do
  vty <- getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.Mouse) $
    liftIO $ V.setMode output V.Mouse True

handleEvent :: BrickEvent Name StreamData -> EventM Name St ()
handleEvent e@VtyEvent{} = do
  mode <- use stMode
  case mode of
    NormalMode -> handleNormal e
    SEditMode -> handleSEdit e
    MEditMode -> handleMEdit e
    SelChatMode -> handleSelChat e
    SpcMenuMode -> handleSpcMenu e
    HelpMenuMode -> handleHelpMenu e
    LoadPickerMode -> handleFilePicker e
    SavePickerMode -> handleFilePicker e
    ModelSelectMode -> handleModelSelector e
    SysPromptMode -> handleFloatEditor editSystemPrompt e
    SelChatEditMode -> handleFloatEditor editSelectChat e

handleEvent (AppEvent delta) = do
  chats <- uses stChats reverse
  let deltaMsg = case delta of
        StreamDelta d -> d
        StreamUsage _ -> ""
        StreamError _ -> ""
        StreamComplete -> ""
  let chats' = case chats of
        (ChatMessage AssistantRole c):xs ->
          ChatMessage AssistantRole (c <> deltaMsg) : xs
        xs -> ChatMessage AssistantRole deltaMsg : xs
  stChats .= reverse chats'
  case delta of
    StreamDelta _ -> scrollChatListEnd
    StreamUsage u -> stStatusInfo .= showUsage u
    StreamError e -> do
      stStatusInfo .= e
      stStream .= Nothing
    _ -> stStream .= Nothing -- reset stream handler

handleEvent (MouseDown ChatList b [] _) = case b of
  V.BScrollUp   -> vScrollBy (viewportScroll ChatList) (-2)
  V.BScrollDown -> vScrollBy (viewportScroll ChatList) 2
  _             -> return ()
handleEvent (MouseDown PreviewList b [] _) = case b of
  V.BScrollUp   -> vScrollBy (viewportScroll PreviewList) (-2)
  V.BScrollDown -> vScrollBy (viewportScroll PreviewList) 2
  _             -> return ()
handleEvent (MouseDown UserEditor b [] _) = case b of
  V.BScrollUp   -> zoom stUserEditor editorUp
  V.BScrollDown -> zoom stUserEditor editorDown
  _             -> return ()
handleEvent (MouseDown FloatEditor b [] _) = case b of
  V.BScrollUp   -> zoom stFloatEditor editorUp
  V.BScrollDown -> zoom stFloatEditor editorDown
  _             -> return ()

handleEvent _ = return ()

handleNormal :: BrickEvent Name e -> EventM Name St ()
handleNormal (VtyEvent (EvKey k [])) = case k of
  KChar 'q' -> halt
  KChar 'j' -> vScrollBy (viewportScroll ChatList) 1
  KChar 'k' -> vScrollBy (viewportScroll ChatList) (-1)
  KDown     -> vScrollBy (viewportScroll ChatList) 1
  KUp       -> vScrollBy (viewportScroll ChatList) (-1)
  KChar 'i' -> changeMode SEditMode
  KChar 'o' -> changeMode MEditMode
  KChar 's' -> unlessStream $ changeMode SelChatMode >> scrollByChatIndex 0
  KChar 'r' -> rerollLastChat
  KChar 'R' -> reEditUserMessage
  KChar 'c' -> stUserEditor .= userEditor ""
  KChar ' ' -> changeMode SpcMenuMode
  KChar '?' -> changeMode HelpMenuMode
  KChar 'p' -> doPasteText
  KChar 'y' -> doYankLast
  KChar 'Y' -> doYankInsc
  KEnter    -> postUserMessage
  _         -> return ()
handleNormal (VtyEvent (EvKey k [V.MCtrl])) = case k of
  KChar 'd' -> vScrollHalf ChatList 1
  KChar 'u' -> vScrollHalf ChatList (-1)
  KChar 'c' -> doCancelStream
  _         -> return ()
handleNormal _ = return ()

handleSEdit :: BrickEvent Name e -> EventM Name St ()
handleSEdit (VtyEvent key@(EvKey k [])) = case k of
  KEsc -> changeMode NormalMode
  KEnter -> postUserMessage
  _    -> zoom stUserEditor $ handleEditor key
handleSEdit (VtyEvent (EvKey k [V.MCtrl])) = case k of
  KChar 'o' -> changeMode MEditMode
  KChar 'c' -> doCancelStream
  _         -> return ()
handleSEdit _ = return ()

handleMEdit :: BrickEvent Name e -> EventM Name St ()
handleMEdit (VtyEvent key@(EvKey k [])) = case k of
  KEsc -> changeMode NormalMode
  _    -> zoom stUserEditor $ handleEditor key
handleMEdit (VtyEvent (EvKey k [V.MCtrl])) = case k of
  KChar 'o' -> changeMode SEditMode
  KChar 'c' -> doCancelStream
  _         -> return ()
handleMEdit _ = return ()

handleSelChat :: BrickEvent n e -> EventM Name St ()
handleSelChat (VtyEvent (EvKey k [])) = case k of
  KEsc      -> changeMode NormalMode >> stChatSelect .= Nothing
  KChar 'j' -> scrollByChatIndex 1
  KChar 'k' -> scrollByChatIndex (-1)
  KChar 'J' -> vScrollBy (viewportScroll ChatList) 1
  KChar 'K' -> vScrollBy (viewportScroll ChatList) (-1)
  KDown     -> vScrollBy (viewportScroll ChatList) 1
  KUp       -> vScrollBy (viewportScroll ChatList) (-1)
  KChar 'i' -> whenJustM (use stChatSelect) $ \n -> do
    t <- use $ stChats . ix n . cmContent
    stFloatEditor .= floatEditor t
    changeMode SelChatEditMode
  KChar 'e' -> whenJustM (use stChatSelect) $ \n -> do
    cfg <- use stConfig
    s <- use $ stChats . ix n . cmContent
    s' <- suspendExt $ editTextExt cfg s
    stChats . ix n . cmContent .= s'
    stChatSelect .= Nothing
    changeMode NormalMode
  KChar 'r' -> whenJustM (use stChatSelect) $ \n -> do
    chats <- uses stChats $ take (n + 1)
    completeChat chats
    stChatSelect .= Nothing
    changeMode NormalMode
  KChar 'y' -> whenJustM (use stChatSelect) $ \n -> do
    cfg <- use stConfig
    s <- use $ stChats . ix n . cmContent
    res <- liftIO (yankText cfg s)
    stStatusInfo .= res
    stChatSelect .= Nothing
    changeMode NormalMode
  _    -> return ()
handleSelChat (VtyEvent (EvKey k [V.MCtrl])) = case k of
  KChar 'd' -> vScrollHalf ChatList 1
  KChar 'u' -> vScrollHalf ChatList (-1)
  _    -> return ()
handleSelChat _ = return ()

handleSpcMenu :: BrickEvent Name e -> EventM Name St ()
handleSpcMenu (VtyEvent (EvKey k [])) = case k of
  KEsc      -> changeMode NormalMode
  KChar 'n' -> doNewChat >> changeMode NormalMode
  KChar 'p' -> doEditSystemPrompt
  KChar 'l' -> doFilePicker LoadPickerMode
  KChar 's' -> doFilePicker SavePickerMode
  KChar 'm' -> unlessStream $ changeMode ModelSelectMode
  _ -> return ()
handleSpcMenu _ = return ()

handleHelpMenu :: BrickEvent Name e -> EventM Name St ()
handleHelpMenu (VtyEvent (EvKey KEsc [])) = changeMode NormalMode
handleHelpMenu _ = return ()

handleFilePicker :: BrickEvent Name e -> EventM Name St ()
handleFilePicker ev@(VtyEvent (EvKey k [])) = case k of
  KEsc      -> changeMode NormalMode
  KUp       -> stFileList %= listMoveBy (-1) >> updatePreview
  KDown     -> stFileList %= listMoveBy 1 >> updatePreview
  KEnter    -> doPickerResult >> changeMode NormalMode
  _         -> zoom stPathEditor $ handleEditorEvent ev
handleFilePicker ev@(VtyEvent (EvKey _ [V.MShift])) =
               zoom stPathEditor $ handleEditorEvent ev
handleFilePicker (VtyEvent (EvKey k [V.MCtrl])) = case k of
  KUp       -> vScrollBy (viewportScroll PreviewList) (-2)
  KDown     -> vScrollBy (viewportScroll PreviewList) 2
  KChar 'd' -> vScrollHalf PreviewList 1
  KChar 'u' -> vScrollHalf PreviewList (-1)
  _         -> return ()
handleFilePicker _ = return ()

handleModelSelector :: BrickEvent Name e -> EventM Name St ()
handleModelSelector (VtyEvent (EvKey k [])) = case k of
  KEsc      -> changeMode NormalMode
  KChar 'k' -> stModelList %= listMoveBy (-1)
  KChar 'j' -> stModelList %= listMoveBy 1
  KUp       -> stModelList %= listMoveBy (-1)
  KDown     -> stModelList %= listMoveBy 1
  KEnter    -> unlessStream $
    whenJustM(uses stModelList listSelectedElement) $ \(_, (p, _)) -> do
      cfg <- use stConfig
      let apiCfg = lookupApi (Just p) $ cfg^.cfgApis
      complete <- liftIO $ chatCompletion apiCfg
      stApiConfig .= apiCfg
      stChatComplete .= complete
      changeMode NormalMode
  _         -> return ()
handleModelSelector _ = return ()

handleFloatEditor :: (Maybe Text -> EventM Name St ())
  -> BrickEvent Name e -> EventM Name St ()
handleFloatEditor h (VtyEvent key@(EvKey k [])) = case k of
  KEsc  -> h Nothing
  _     -> zoom stFloatEditor $ handleEditor key
handleFloatEditor h (VtyEvent (EvKey k [V.MCtrl])) = case k of
  KChar 's' -> do
    sp <- floatEditorText
    h $ Just sp
  KChar 'e' -> do
    cfg <- use stConfig
    s <- floatEditorText
    s' <- suspendExt $ editTextExt cfg s
    stFloatEditor .= floatEditor s'
  KChar 'r' -> do
    stFloatEditor .= floatEditor ""
  _ -> return ()
handleFloatEditor _ _ = return ()

floatEditorText :: EventM Name St Text
floatEditorText = uses stFloatEditor $ Text.strip . (Text.unlines <$> dumpEditor)

editSystemPrompt :: Maybe Text -> EventM Name St ()
editSystemPrompt t = do
  whenJust t $ \s -> do
    chats <- uses stChats $ filter (views cmRole (/= SystemRole))
    stChats .= [ChatMessage SystemRole s | not $ T.null s] <> chats
  changeMode NormalMode

editSelectChat :: Maybe Text -> EventM Name St ()
editSelectChat t = do
  whenJustM (use stChatSelect) $ \idx -> do
    whenJust t $ \s -> do
      stChats . ix idx . cmContent .= s
  changeMode SelChatMode

postUserMessage :: EventM Name St ()
postUserMessage = unlessStream $ do
  txts <- uses stUserEditor $ Text.strip . (Text.unlines <$> dumpEditor)
  unless (Text.null txts) $ do
    stChats <>= [ChatMessage UserRole txts]
    vScrollToEnd $ viewportScroll ChatList
    stUserEditor .= userEditor ""
    doChatComplete

rerollLastChat :: EventM Name St ()
rerollLastChat = unlessStream $ do
  use stChats >>= completeChat

completeChat :: [ChatMessage] -> EventM Name St ()
completeChat chats = do
  let f c = case reverse c of
        (ChatMessage AssistantRole _):xs -> reverse $ f xs
        _                                -> c
  stChats .= f chats
  doChatComplete

reEditUserMessage :: EventM Name St ()
reEditUserMessage = do
  -- drop the assistant message if possible
  let drops = dropWhile $ views cmRole (== AssistantRole)
  chats <- uses stChats $ drops . reverse
  case chats of
    (ChatMessage UserRole c):xs -> do
      stChats .= reverse xs
      vScrollToEnd $ viewportScroll ChatList
      stUserEditor .= userEditor c -- re-edit user message
    _ -> return () -- do nothing

  return ()

doEditSystemPrompt :: EventM Name St ()
doEditSystemPrompt = do
  chat <- use stChats
  let sp = case chat of
        (ChatMessage SystemRole c):_ -> c
        _                            -> ""
  stFloatEditor .= floatEditor sp
  changeMode SysPromptMode

makeMessages :: [ChatMessage] -> [Message]
makeMessages = fmap f where
  f (ChatMessage role content) = message (g role) content
  g SystemRole = "system"
  g UserRole = "user"
  g AssistantRole = "assistant"

unlessStream :: EventM Name St () -> EventM Name St ()
unlessStream m = do
  cancel <- use stStream
  when (isNothing cancel) m

changeMode :: Mode -> EventM Name St ()
changeMode mode = do
  let testIME = (`elem` [SEditMode, MEditMode, SysPromptMode, SelChatEditMode])
      s1 = testIME mode
  s0 <- uses stMode testIME
  stMode .= mode
  when (s0 /= s1) $ do
    cfg <- use stConfig
    liftIO $ selectIME cfg s1

scrollByChatIndex :: Int -> EventM Name St ()
scrollByChatIndex d = do
  len <- uses stChats $ \x ->
    if null x then Nothing else Just (length x)
  let f Nothing  = pred <$> len
      f (Just n) = let p = n + d
                   in max 0 . min p . pred <$> len
  stChatSelect %= f
  msgs <- uses stChats $ fmap (^. cmContent)
  sel <- use stChatSelect
  case sel of
    Just idx -> do
      pos <- chatPosOf ChatList bubbleAttr idx msgs
      setTop (viewportScroll ChatList) pos
    Nothing -> return ()

vScrollHalf :: Eq n => n -> Int -> EventM n s ()
vScrollHalf n d = do
  ext <- lookupExtent n
  let h = case ext of
        Just e -> snd $ extentSize e
        Nothing -> 0
      dl = d * max 1 (h `div` 2)
  vScrollBy (viewportScroll n) dl

scrollChatListEnd :: EventM Name s ()
scrollChatListEnd = do
  whenJustM (lookupViewport ChatList) $ \vp ->
    when (vp^.vpTop + vp ^. vpSize . _2 >= vp^.vpContentSize . _2) $
      vScrollToEnd $ viewportScroll ChatList

doChatComplete :: EventM Name St ()
doChatComplete = unlessStream $ do
  complete <- use stChatComplete
  chan <- use stEventChan
  msgs <- uses stChats makeMessages
  cancel <- liftIO $ complete msgs $ writeBChan chan
  stStream .= Just cancel
  scrollChatListEnd

doCancelStream :: EventM Name St ()
doCancelStream = do
  s <- use stStream
  liftIO $ maybe (return ()) cancelStream s

doNewChat :: EventM Name St ()
doNewChat = unlessStream $ do
  chats <- use stChats
  stChats .= case chats of
    x@(ChatMessage SystemRole _):_ -> [x]
    _ -> []
  vScrollToEnd $ viewportScroll ChatList
  stUserEditor .= userEditor ""

doYankInsc :: EventM Name St ()
doYankInsc = do
  cfg <- use stConfig
  chats <- use stChats
  res <- liftIO $ yankInsc cfg chats
  stStatusInfo .= res

doYankLast :: EventM Name St ()
doYankLast = do
  chats <- uses stChats reverse
  case chats of
    x:_ -> do
      cfg <- use stConfig
      s <- liftIO $ yankText cfg x._cmContent
      assign stStatusInfo s
    _   -> return ()

doPasteText :: EventM Name St ()
doPasteText = do
  r <- use stConfig >>= liftIO . pasteText
  case r of
    Right t -> zoom stUserEditor $ editorAppend t
    Left e -> stStatusInfo .= e

doFilePicker :: Mode -> EventM Name St ()
doFilePicker mode = do
  r <- uses stFileList $ fmap snd . listSelectedElement
  files <- liftIO $ scanSessionFiles ""
  let f = maybe id (listFindBy . (\x -> (== x)))
  stFileList .= f r (fileList files)
  changeMode mode
  updatePreview

doPickerResult :: EventM Name St ()
doPickerResult = do
  mode <- use stMode
  path <- uses stPathEditor $ concat . BE.getEditContents
  case mode of
    LoadPickerMode -> do
      use stPreviewChat >>= (stChats .=)
      vScrollToEnd $ viewportScroll ChatList
    SavePickerMode -> use stChats >>= liftIO . saveIns path
    _              -> return ()
  return ()

updatePreview :: EventM Name St ()
updatePreview = do
  r <- uses stFileList listSelectedElement
  case r of
    Just (_, p) -> do
      stPathEditor .= pathEditor p
      chat <- liftIO $ loadIns p
      stPreviewChat .= chat
    Nothing -> stPreviewChat .= []

suspendExt :: Ord n => IO a -> EventM n s a
suspendExt m = suspendAndResume' m <* startEvent
