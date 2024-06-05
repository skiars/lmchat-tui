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

-- | This module is for internal use.

{-# LANGUAGE Safe #-}

module WEditor.Internal.Line (
  EditingLine,
  appendToLine,
  atLineBack,
  atLineFront,
  editLine,
  getLineCursor,
  joinLines,
  lineCursorMovable,
  modifyLine,
  moveLineCursor,
  prependToLine,
  setLineCursor,
  splitLineAtCursor,
  viewLine,
) where

import WEditor.Base.Editor
import WEditor.Base.Line

import Prelude hiding (null, reverse, head, tail)
import Data.Text (Text, cons, null, reverse, head, tail)
import qualified Data.Text as T

data EditingLine b =
  EditingLine {
    elTextBefore :: Text,  -- Reversed.
    elTextAfter :: Text,
    elBreak :: b
  }
  deriving (Eq,Show)

editLine :: VisibleLine b -> EditingLine b
editLine (VisibleLine b cs) = EditingLine mempty cs b

viewLine :: EditingLine b -> VisibleLine b
viewLine (EditingLine bs as b) = VisibleLine b (reverse bs <> as)

joinLines :: [VisibleLine b] -> Text
joinLines = mconcat . fmap vlText

getLineCursor :: EditingLine b -> Int
getLineCursor = T.length . elTextBefore

setLineCursor :: Int -> EditingLine b -> EditingLine b
setLineCursor k (EditingLine bs as b) = EditingLine bs2 as2 b where
  (bs2,as2) = seek (T.length bs) bs as
  seek n bs as
    | k < n && not (null bs) = seek (n-1) (tail bs)           (head bs `cons` as)
    | k > n && not (null as) = seek (n+1) (head as `cons` bs) (tail as)
    | otherwise = (bs,as)

splitLineAtCursor :: (Int -> VisibleLine b -> (VisibleLine b,VisibleLine b))
                  -> EditingLine b -> (VisibleLine b,VisibleLine b)
splitLineAtCursor f l@(EditingLine bs _ _) = f (T.length bs) (viewLine l)

lineCursorMovable :: MoveDirection -> EditingLine b -> Bool
lineCursorMovable MovePrev (EditingLine bs _ _) = not $ null bs
lineCursorMovable MoveNext (EditingLine _ as _) = not $ null as
lineCursorMovable MoveHome _ = True
lineCursorMovable MoveEnd  _ = True
lineCursorMovable _ _ = False

moveLineCursor :: MoveDirection -> EditingLine b -> EditingLine b
moveLineCursor MoveHome p = moveLineCursor MoveUp   p
moveLineCursor MoveEnd  p = moveLineCursor MoveDown p
moveLineCursor MoveUp (EditingLine bs as b) =
  EditingLine mempty (reverse bs <> as) b
moveLineCursor MoveDown (EditingLine bs as b) =
  EditingLine (reverse as <> bs) mempty b
moveLineCursor MovePrev (EditingLine bs as b)
  | not (null bs) = EditingLine (tail bs) (head bs `cons` as) b
moveLineCursor MoveNext (EditingLine bs as b)
  | not (null as) = EditingLine (head as `cons` bs) (tail as) b
moveLineCursor _ l = l

atLineFront :: EditingLine b -> Bool
atLineFront = null . elTextBefore

atLineBack :: EditingLine b -> Bool
atLineBack = null . elTextAfter

appendToLine :: EditingLine b -> VisibleLine b -> EditingLine b
appendToLine (EditingLine bs as _) (VisibleLine b cs) =
  EditingLine bs (as <> cs) b

prependToLine :: VisibleLine b -> EditingLine b -> EditingLine b
prependToLine (VisibleLine _ cs) (EditingLine bs as b) =
  EditingLine (bs <> reverse cs) as b

modifyLine :: EditAction -> EditDirection -> EditingLine b -> EditingLine b
modifyLine (InsertText cs) d (EditingLine bs as b) = revised where
  bs2 = if d == EditBefore
           then reverse cs <> bs
           else bs
  as2 = if d == EditAfter
           then cs <> as
           else as
  revised = EditingLine bs2 as2 b
modifyLine DeleteText d (EditingLine bs as b) = revised where
  bs2 = if d == EditBefore && not (null bs)
           then tail bs
           else bs
  as2 = if d == EditAfter && not (null as)
           then tail as
           else as
  revised = EditingLine bs2 as2 b
