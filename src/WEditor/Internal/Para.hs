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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module WEditor.Internal.Para (
  EditingPara,
  VisibleParaAfter,
  VisibleParaBefore,
  appendToPara,
  atParaBack,
  atParaFront,
  catLinesAfter,
  catLinesBefore,
  countLinesAfter,
  countLinesBefore,
  editPara,
  getAfterLines,
  getBeforeLines,
  getCurrentLine,
  getParaCharCount,
  getParaCursorChar,
  getParaCursorLine,
  getParaEditChar,
  modifyPara,
  moveParaCursor,
  paraCursorMovable,
  parseParaAfter,
  parseParaBefore,
  prependToPara,
  reparsePara,
  seekParaBack,
  seekParaFront,
  setParaCursorChar,
  setParaEditChar,
  splitPara,
  unparsePara,
  unparseParaAfter,
  unparseParaBefore,
  viewAfterLines,
  viewBeforeLines,
  viewParaAfter,
  viewParaBefore,
) where

import WEditor.Base.Editor
import WEditor.Base.Line
import WEditor.Base.Para
import WEditor.Base.Parser
import WEditor.Internal.Line

import qualified Data.Text as T

data VisibleParaBefore b =
  VisibleParaBefore {
    vpbLines :: [VisibleLine b],  -- Reversed.
    vpbSize :: Int
  }
  deriving (Show)

data VisibleParaAfter b =
  VisibleParaAfter {
    vpaLines :: [VisibleLine b]
  }
  deriving (Show)

data EditingPara b =
  EditingPara {
    epBefore :: [VisibleLine b],  -- Reversed.
    epEditing :: EditingLine b,
    epAfter :: [VisibleLine b],
    epSizeBefore :: Int
  }
  deriving (Show)

viewBeforeLines :: VisibleParaBefore b -> [VisibleLine b]
viewBeforeLines = reverse . vpbLines

viewAfterLines :: VisibleParaAfter b -> [VisibleLine b]
viewAfterLines = vpaLines

visibleParaBefore :: [VisibleLine b] -> VisibleParaBefore b
visibleParaBefore ls = VisibleParaBefore ls (sum $ map (T.length . vlText) ls)

parseParaBefore :: FixedFontParser a => a -> UnparsedPara -> VisibleParaBefore (BreakType a)
parseParaBefore parser (UnparsedPara cs) =
  VisibleParaBefore (reverse $ breakLines parser cs) (T.length cs)

parseParaAfter :: FixedFontParser a => a -> UnparsedPara -> VisibleParaAfter (BreakType a)
parseParaAfter parser (UnparsedPara cs) = VisibleParaAfter $ breakLines parser cs

unparseParaBefore :: VisibleParaBefore b -> UnparsedPara
unparseParaBefore (VisibleParaBefore ls _) = UnparsedPara $ joinLines $ reverse ls

unparseParaAfter :: VisibleParaAfter b -> UnparsedPara
unparseParaAfter (VisibleParaAfter ls) = UnparsedPara $ joinLines ls

editPara :: FixedFontParser a => a -> UnparsedPara -> EditingPara (BreakType a)
editPara parser (UnparsedPara cs) = EditingPara [] (editLine line) after 0 where
  (line:after) = nonempty $ breakLines parser cs
  nonempty [] = [emptyLine parser]
  nonempty ls = ls

unparsePara :: EditingPara b -> UnparsedPara
unparsePara (EditingPara bs l as _) = UnparsedPara $ joinLines ls where
  ls = reverse bs ++ [viewLine l] ++ as

reparsePara :: FixedFontParser a =>
  a -> EditingPara (BreakType a) -> EditingPara (BreakType a)
reparsePara parser (EditingPara bs l as n) = reparseParaTail parser revised where
  revised = EditingPara bs2 l2 as (sum $ map (T.length . vlText) bs2)
  bs' = reverse $ breakLines parser $ joinLines (reverse bs)
  (l2,bs2)
    | null bs' = (l,[])
    | otherwise = (head bs' `prependToLine` l,tail bs')

viewParaBefore :: EditingPara b -> VisibleParaBefore b
viewParaBefore (EditingPara bs l as _) = visibleParaBefore ls where
  ls = reverse as ++ [viewLine l] ++ bs

viewParaAfter :: EditingPara b -> VisibleParaAfter b
viewParaAfter (EditingPara bs l as _) = VisibleParaAfter ls where
  ls = reverse bs ++ [viewLine l] ++ as

getBeforeLines :: EditingPara b -> VisibleParaBefore b
getBeforeLines = visibleParaBefore . epBefore

getCurrentLine :: EditingPara b -> VisibleLine b
getCurrentLine = viewLine . epEditing

getAfterLines :: EditingPara b -> VisibleParaAfter b
getAfterLines = VisibleParaAfter . epAfter

catLinesBefore :: [VisibleParaBefore b] -> [VisibleLine b]
catLinesBefore = concat . map vpbLines

catLinesAfter :: [VisibleParaAfter b] -> [VisibleLine b]
catLinesAfter = concat . map vpaLines

countLinesBefore :: Int -> [VisibleParaBefore b] -> Int
countLinesBefore n = length . take n . catLinesBefore

countLinesAfter :: Int -> [VisibleParaAfter b] -> Int
countLinesAfter n = length . take n . catLinesAfter

getParaCursorLine :: EditingPara b -> Int
getParaCursorLine = length . epBefore

getParaCursorChar :: EditingPara b -> Int
getParaCursorChar = getLineCursor . epEditing

setParaCursorChar :: Int -> EditingPara b -> EditingPara b
setParaCursorChar k e@(EditingPara bs l as n) = (EditingPara bs (setLineCursor k l) as n)

getParaCharCount :: EditingPara b -> Int
getParaCharCount = T.length . upText . unparsePara

getParaEditChar :: EditingPara b -> Int
getParaEditChar (EditingPara _ l _ n) = n + getLineCursor l

setParaEditChar :: Int -> EditingPara b -> EditingPara b
setParaEditChar k p
  | getParaEditChar p > k && not (atParaFront p) = setParaEditChar k $ moveParaCursor MovePrev p
  | getParaEditChar p < k && not (atParaBack p)  = setParaEditChar k $ moveParaCursor MoveNext p
  | otherwise = p

splitPara :: FixedFontParser a => a -> EditingPara (BreakType a) -> (UnparsedPara, UnparsedPara)
splitPara parser (EditingPara bs l as _) = let (b,a) = splitLineAtCursor (splitLine parser) l in
  (unparseParaBefore $ VisibleParaBefore (b:bs) 0,
   unparseParaAfter  $ VisibleParaAfter  (a:as))

paraCursorMovable :: MoveDirection -> EditingPara b -> Bool
paraCursorMovable d
  | d == MoveUp   = not . atParaTop
  | d == MoveDown = not . atParaBottom
  | d == MovePrev = not . atParaFront
  | d == MoveNext = not . atParaBack
  | d == MoveHome || d == MoveEnd = const True
  | otherwise = const False

moveParaCursor :: MoveDirection -> EditingPara b -> EditingPara b
moveParaCursor d p@(EditingPara bs l as n) = revised where
  revised
    | d == MoveHome || d == MoveEnd   = EditingPara bs (moveLineCursor d l)        as n
    | d == MoveUp   && atParaTop p    = EditingPara bs (moveLineCursor MoveUp l)   as n
    | d == MoveDown && atParaBottom p = EditingPara bs (moveLineCursor MoveDown l) as n
    | not (paraCursorMovable d p) = p
    | d == MoveUp   =
      setParaCursorChar (getLineCursor l) $
      EditingPara (tail bs) (editLine $ head bs) (viewLine l:as) (n - T.length (vlText $ head bs))
    | d == MoveDown =
      setParaCursorChar (getLineCursor l) $
      EditingPara (viewLine l:bs) (editLine $ head as) (tail as) (n + T.length (vlText $ viewLine l))
    | lineCursorMovable d l = EditingPara bs (moveLineCursor d l) as n
    | d == MovePrev = setBack  $ moveParaCursor MoveUp   p
    | d == MoveNext = setFront $ moveParaCursor MoveDown p
  setBack  (EditingPara bs l as n) = (EditingPara bs (moveLineCursor MoveDown l) as n)
  setFront (EditingPara bs l as n) = (EditingPara bs (moveLineCursor MoveUp   l) as n)

atParaFront :: EditingPara b -> Bool
atParaFront p@(EditingPara _ l _ _) = atParaTop p && atLineFront l

atParaBack :: EditingPara b -> Bool
atParaBack p@(EditingPara _ l _ _) = atParaBottom p && atLineBack l

seekParaFront :: EditingPara b -> EditingPara b
seekParaFront p
  | atParaFront p = p
  | otherwise     = seekParaFront $ moveParaCursor MoveUp p

seekParaBack :: EditingPara b -> EditingPara b
seekParaBack p
  | atParaBack p = p
  | otherwise    = seekParaBack $ moveParaCursor MoveDown p

appendToPara :: FixedFontParser a
  => a -> EditingPara (BreakType a) -> VisibleParaAfter (BreakType a) -> EditingPara (BreakType a)
appendToPara parser (EditingPara bs l as n) (VisibleParaAfter cs) = reparseParaTail parser revised where
  revised = EditingPara bs l (as ++ cs) n

prependToPara :: FixedFontParser a
  => a -> VisibleParaBefore (BreakType a) -> EditingPara (BreakType a) -> EditingPara (BreakType a)
prependToPara parser (VisibleParaBefore cs _) (EditingPara bs l as _) = reparseParaTail parser revised where
  revised = EditingPara bs2 l2 as n2
  (VisibleParaBefore bs' n') = parseParaBefore parser $ unparseParaBefore $ visibleParaBefore (bs ++ cs)
  (l2,bs2,n2) = if null bs'
                   then (l,[],0)
                   else (head bs' `prependToLine` l,tail bs',n' - T.length (vlText $ head bs'))

modifyPara :: FixedFontParser a
  => a -> EditAction -> EditDirection -> EditingPara (BreakType a) -> EditingPara (BreakType a)
modifyPara parser m d p = reparseParaTail parser revised where
  (EditingPara bs l as n) = mergeForEdit p
  revised = EditingPara bs (modifyLine m d l) as n


-- Private below here.

reparseParaTail :: FixedFontParser a
  => a -> EditingPara (BreakType a) -> EditingPara (BreakType a)
reparseParaTail parser p@(EditingPara bs l as n) = setParaEditChar offset revised where
  offset = getParaEditChar p
  revised = EditingPara bs (editLine line) after n
  (line:after) = breakLines parser $ joinLines (viewLine l:as)

mergeForEdit :: EditingPara b -> EditingPara b
mergeForEdit (EditingPara bs l as n) = EditingPara bs2 l2 as2 n2 where
  l2 = addAfter as $ addBefore bs l where
    addAfter (v:_) l = l `appendToLine` v
    addAfter _ l = l
    addBefore (v:_) l = v `prependToLine` l
    addBefore _ l = l
  bs2 = if null bs then [] else tail bs
  as2 = if null as then [] else tail as
  n2 = n - (if null bs then 0 else T.length (vlText $ head bs))

atParaTop :: EditingPara b -> Bool
atParaTop (EditingPara bs _ _ _) = null bs

atParaBottom :: EditingPara b -> Bool
atParaBottom (EditingPara _ _ as _) = null as
