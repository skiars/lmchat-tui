{- -----------------------------------------------------------------------------
Copyright 2020 Kevin P. Barry

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
----------------------------------------------------------------------------- -}

-- Author: Kevin P. Barry [ta0kira@gmail.com]

-- | A wrapping text-editor with dynamic sizing for
--   <https://github.com/jtdaugherty/brick Brick>.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}

module WEditorBrick.WrappingEditor (
  WrappingEditor,
  WrappingEditorAction,
  WrappingEditorDoer,
  BubbleAlign (..),
  BubbleAttr (..),
  doEditor,
  dumpEditor,
  genericEditor,
  handleEditor,
  mapEditor,
  newEditor,
  editorAppend,
  editorDown,
  editorUp,
  renderEditor,
  updateEditorExtent,
  txtWrap,
  txtBubble,
  leftBubbleAttr,
  rightBubbleAttr,
  bubbleAttr,
  chatPosOf
) where

import Brick.Main
import Brick (AttrName, attrName)
import Brick.Types hiding (zoom)
import Brick.Widgets.Core hiding (textWidth, txtWrap)
import Graphics.Vty.Input
import Control.Lens hiding (view)
import qualified Data.Text as T
import WEditor.Base
import WEditor.Document
import WEditor.LineWrap

-- | Create a new 'WrappingEditor' using the default editor component.
newEditor :: FixedFontParser p => p -> n -> Text -> WrappingEditor n
newEditor b n = genericEditor n . appendG e0 where
  e0 = viewerResizeAction (8, 1000000) $ editDocument b []

editorAppend :: Eq n => Text -> EventM n (WrappingEditor n) ()
editorAppend t = handle (`appendG` t)

editorDown :: Eq n => EventM n (WrappingEditor n) ()
editorDown = handle editorDownAction

editorUp :: Eq n => EventM n (WrappingEditor n) ()
editorUp = handle editorUpAction

appendG :: FixedFontEditor c => c -> Text -> c
appendG e0 = f e0 . T.lines where
  f e []     = e
  f e [x]    = app x e
  f e (x:xs) = f (editorEnterAction $ app x e) xs
  app x = editorAppendAction (T.filter (`notElem` ("\t\r\n" :: String)) x)

-- | Create a new 'WrappingEditor' using a custom editor component.
genericEditor :: (FixedFontViewer e, FixedFontEditor e) => n -> e -> WrappingEditor n
genericEditor = WrappingEditor

-- | Any action that updates the editor state.
type WrappingEditorAction = forall e. (FixedFontViewer e, FixedFontEditor e) => e -> e

-- | Update the editor state.
mapEditor :: WrappingEditorAction -> WrappingEditor n -> WrappingEditor n
mapEditor f (WrappingEditor name editor) = WrappingEditor name (f editor)

-- | Any action that reads the editor state.
type WrappingEditorDoer b = forall e. (FixedFontViewer e, FixedFontEditor e) => e -> b

-- | Read from the editor state.
doEditor :: WrappingEditorDoer b -> WrappingEditor n -> b
doEditor f (WrappingEditor _ editor) = f editor

-- | Dump the final contents of the edited document.
dumpEditor :: WrappingEditor n -> [Text]
dumpEditor = map upText . doEditor exportData

-- | Render the editor as a 'Widget'.
renderEditor :: (Ord n, Show n) => Bool -> WrappingEditor n -> Widget n
renderEditor focus editor = doEditor view editor where
  view e = Widget Greedy Fixed $ do
    ctx <- getContext
    let width = ctx^.availWidthL
    let height = ctx^.availHeightL
    -- NOTE: Resizing is a no-op if the size is unchanged.
    let e' = if height > 0
                then viewerResizeAction (width,height) e
                else e
    render $ viewport (getName editor) Vertical $ setCursor e' $ textArea width height e' where
      setCursor
        | focus = showCursor (getName editor) . Location . getCursor
        | otherwise = const id
      textArea w h = vBox . lineFill w h . fmap (textFill w) . getVisible
      lineFill w h ls = take h $ ls <> repeat (textFill w "")

-- | Update the viewport size based on the most-recent rendering of the editor.
--
--   Call this before any custom event-handling logic so that the viewport is
--   the correct size. This will ensure that vertical cursor movements match
--   what the user expects.
updateEditorExtent :: Eq n => EventM n (WrappingEditor n) ()
updateEditorExtent = do
  editor <- get
  extent <- lookupExtent (getName editor)
  put $ mapEditor (resize extent) editor where
    resize (Just ext) | snd (extentSize ext) > 0 = viewerResizeAction (extentSize ext)
    resize  _ = id

handle :: Eq n => WrappingEditorAction -> EventM n (WrappingEditor n) ()
handle act = updateEditorExtent >> modify (mapEditor act)

-- | Update the editor based on Brick events.
--
--   In addition to the canonical typing events, this handler also supports:
--
--     * @PageUp@, @PageDown@, @Home@, and @End@ keys.
--     * @Alt@+@Up@ shifts the view upward one line.
--     * @Alt@+@Down@ shifts the view downward one line.
--     * @Alt@+@Home@ shifts the view to hide empty space at the bottom.
--
--   To disable or override any of these keys, intercept them in the main
--   handler for the 'App'.
handleEditor :: Eq n => Event -> EventM n (WrappingEditor n) ()
handleEditor event = handle action where
  action :: WrappingEditorAction
  action = case event of
    EvKey KBS []        -> editorBackspaceAction
    EvKey KDel []       -> editorDeleteAction
    EvKey KDown []      -> editorDownAction
    EvKey KEnd []       -> editorEndAction
    EvKey KEnter []     -> editorEnterAction
    EvKey KHome []      -> editorHomeAction
    EvKey KLeft []      -> editorLeftAction
    EvKey KPageDown []  -> editorPageDownAction
    EvKey KPageUp []    -> editorPageUpAction
    EvKey KRight []     -> editorRightAction
    EvKey KUp []        -> editorUpAction
    EvKey KDown [MMeta] -> viewerShiftDownAction 1
    EvKey KUp [MMeta]   -> viewerShiftUpAction   1
    EvKey KHome [MMeta] -> viewerFillAction
    EvKey (KChar c) [] | c `notElem` ("\t\r\n" :: String) -> editorAppendAction (T.singleton c)
    _ -> id

-- | Editor widget for use with Brick.
data WrappingEditor n =
  forall e. (FixedFontViewer e, FixedFontEditor e) => WrappingEditor {
    weName :: n,
    weEditor :: e
  }

instance Show n => Show (WrappingEditor n) where
  show (WrappingEditor name editor) =
    "WrappingEditor { name: " ++ show name ++
                   ", size: " ++ show (getViewSize editor) ++
                   ", cursor: " ++ show (getCursor editor) ++
                   ", point: " ++ show (getEditPoint editor) ++ " }"

instance Named (WrappingEditor n) n where
    getName = weName

txtWrap :: T.Text -> Widget n
txtWrap s = Widget Greedy Fixed $ do
  c <- getContext
  let wl = c^.availWidthL
      theLines = T.lines s >>= wrapTextToLines wl
  case theLines of
      [] -> return emptyResult
      multiple -> render $ vBox $ txt <$> multiple

textFill :: Int -> Text -> Widget n
textFill w cs = txt $ justifyLeft w cs

justifyLeft :: Int -> Text -> Text
justifyLeft w cs = let wt = textWidth cs
                   in  cs <> T.replicate (w - wt) " "

wrapTextToLines :: Int -> Text -> [Text]
wrapTextToLines wl s = vlText <$> brk where
  brk = breakLines (setLineWidth breakExact wl) s

data BubbleAlign = BubbleLeft | BubbleRight deriving (Show, Eq)

newtype BubbleAttr = BubbleAttr {
  bubbleAlign :: BubbleAlign
} deriving Show

bubbleAttr :: BubbleAttr
bubbleAttr = BubbleAttr BubbleLeft

bubbleHLimit :: Int -> Int
bubbleHLimit wa = clamp 80 120 flexW where
  flexW = (wa * 65) `div` 100
  clamp l u x | x < l - 2 = min l $ wa - 2
              | otherwise = min u flexW

txtBubble :: BubbleAttr -> T.Text -> T.Text -> Widget n
txtBubble attr tit s = Widget Greedy Fixed $ do
  c <- getContext
  let wa = c^.availWidthL
      wl = bubbleHLimit wa - 2
      wt = textWidth tit
      theLines = T.lines s >>= wrapTextToLines wl
      ws = max (wt + 2) $ maximum $ textWidth <$> theLines
      pad = case attr.bubbleAlign of
        BubbleLeft -> ""
        BubbleRight -> T.replicate (wa - 2 - ws) " "
      txtBorder = txt . (pad <>) . ("│" <>) . (<> "│") . justifyLeft ws
      topC = case attr.bubbleAlign of
        BubbleLeft -> withAttr leftBubbleAttr (txt tit) <+> txt (T.replicate (ws - wt) "─")
        BubbleRight -> txt (T.replicate (ws - wt) "─") <+> withAttr rightBubbleAttr (txt tit)
      top = txt (pad <> "╭") <+> topC <+> txt "╮"
      bot = pad <> "╰" <> T.replicate ws "─" <> "╯"
      bubble x = vBox $ [top] <> x <> [txt bot]
  case theLines of
      [] -> return emptyResult
      multiple -> render $ bubble $ txtBorder <$> multiple

leftBubbleAttr :: AttrName
leftBubbleAttr = attrName "leftBubble"

rightBubbleAttr :: AttrName
rightBubbleAttr = attrName "rightBubble"

chatPosOf :: Eq n => n -> BubbleAttr -> Int -> [Text] -> EventM n s Int
chatPosOf n _ pos txts = do
  ext <- lookupExtent n
  let wa = case ext of
        Just e -> fst $ extentSize e
        Nothing -> 0
      wl = bubbleHLimit wa - 2
      splitLines s = T.lines s >>= wrapTextToLines wl
      fc 0 h _      = h
      fc _ h []     = h
      fc d h (x:xs) = let h' = h + length (splitLines x) + 2
                      in  fc (d - 1) h' xs
  return $ fc pos 0 txts
