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

-- | Generic document-editing components.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}

module WEditor.Document (
  EditingDocument,
  editDocument,
  -- From Base >>>
  EditAction(..),
  EditDirection(..),
  EditorAction,
  FixedFontEditor(..),
  FixedFontParser,
  FixedFontViewer(..),
  MoveDirection(..),
  UnparsedPara(..),
  ViewerAction,
  editorAppendAction,
  editorBackspaceAction,
  editorDeleteAction,
  editorDownAction,
  editorEndAction,
  editorEnterAction,
  editorHomeAction,
  editorInsertAction,
  editorLeftAction,
  editorPageDownAction,
  editorPageUpAction,
  editorRightAction,
  editorSetPositionAction,
  editorUpAction,
  viewerFillAction,
  viewerResizeAction,
  viewerShiftUpAction,
  viewerShiftDownAction,
  -- <<< From Base
) where

import WEditor.Base
import WEditor.Internal.Para


-- | Generic document editor with a dynamic viewport.
--
--   This editor bounds vertical view scrolling (see 'ViewAction') to keep the
--   cursor within view.
data EditingDocument =
  forall a. FixedFontParser a => EditingDocument {
    edBefore :: [VisibleParaBefore (BreakType a)],  -- Reversed.
    edEditing :: EditingPara (BreakType a),
    edAfter :: [VisibleParaAfter (BreakType a)],
    edWidth :: Int,
    edHeight :: Int,
    edOffset :: Int,
    edLastCursor :: Int,
    edParser :: a
  }

-- | Create an editor for a document.
editDocument :: FixedFontParser a => a                 -- ^ Parser used to break paragraphs into lines.
                                    -> [UnparsedPara]  -- ^ List of unparsed paragraphs to be edited.
                                    -> EditingDocument -- ^ Document editor.
editDocument parser ps = document where
  document = EditingDocument {
      edBefore = [],
      edEditing = editPara parser first,
      edAfter = map (parseParaAfter parser) rest,
      edWidth = 0,
      edHeight = 0,
      edOffset = 0,
      edLastCursor = 0,
      edParser = parser
    }
  (first:rest) = nonempty ps
  nonempty [] = [emptyPara]
  nonempty ps = ps


-- Private below here.

instance Show (EditingDocument) where
  show d =
    "EditingDocument { size: " ++ show (getViewSize d) ++
                    ", cursor: " ++ show (getCursor d) ++
                    ", point: " ++ show (getEditPoint d) ++ " }"

instance FixedFontViewer EditingDocument where
  setViewSize d s@(w,h)
    | w /= edWidth d  = storeCursor $ resizeHeight h $ resizeWidth w d
    | h /= edHeight d = storeCursor $ resizeHeight h d
    | otherwise = d
  getViewSize d = (edWidth d,edHeight d)
  getVisible = getVisibleLines
  updateView = flip updateDocView

instance FixedFontEditor EditingDocument where
  editText da m d = storeCursor $ modifyDoc m d da
  breakPara da d = storeCursor $ insertParaSplit d da
  moveCursor da d = updateCursor $ moveDocCursor d da where
    updateCursor
      | d == MovePrev || d == MoveNext || d == MoveHome || d == MoveEnd = storeCursor
      | otherwise = applyCursor
  getCursor (EditingDocument _ e _ _ h k _ p) =
    (tweakCursor p (getCurrentLine e) $ getParaCursorChar e,boundOffset h k)
  getEditPoint = getDocEditPoint
  setEditPoint da (p,c) = storeCursor $ setDocEditPoint (p,c) da
  getParaSize (EditingDocument _ e _ _ _ _ _ _) = getParaCharCount e
  getParaCount da@(EditingDocument bs _ as _ _ _ _ _) = 1 + length bs + length as
  exportData = exportDocument

exportDocument :: EditingDocument -> [UnparsedPara]
exportDocument (EditingDocument bs e as _ _ _ _ _) =
  reverse (map unparseParaBefore bs) ++ [unparsePara e] ++ (map unparseParaAfter as)

boundOffset :: Int -> Int -> Int
boundOffset h k
  | h < 1 = max 0 k
  | otherwise = max 0 (min (h-1) k)

storeCursor :: EditingDocument -> EditingDocument
storeCursor (EditingDocument bs e as w h k _ p) =
  EditingDocument bs e as w h k (getParaCursorChar e) p

applyCursor :: EditingDocument -> EditingDocument
applyCursor (EditingDocument bs e as w h k c p) =
  EditingDocument bs (setParaCursorChar c e) as w h k c p

joinParaNext :: EditingDocument -> EditingDocument
joinParaNext da@(EditingDocument _ _ [] _ _ _ _ _) = da
joinParaNext (EditingDocument bs e (a:as) w h k c p) =
  EditingDocument bs (appendToPara p e a) as w h k c p

joinParaPrev :: EditingDocument -> EditingDocument
joinParaPrev da@(EditingDocument [] _ _ _ _ _ _ _) = da
joinParaPrev (EditingDocument (b:bs) e as w h k c p) =
  EditingDocument bs (prependToPara p b e) as w h (boundOffset h (k-1)) c p

resizeWidth :: Int -> EditingDocument -> EditingDocument
resizeWidth w (EditingDocument bs e as _ h k c p) = (EditingDocument bs2 e2 as2 w h k c p2) where
  p2 = setLineWidth p w
  bs2 = map (parseParaBefore p2 . unparseParaBefore) bs
  as2 = map (parseParaAfter  p2 . unparseParaAfter)  as
  e2 = reparsePara p2 e

resizeHeight :: Int -> EditingDocument -> EditingDocument
resizeHeight h da@(EditingDocument bs e as w _ k c p) =
  (EditingDocument bs e as w h offset c p) where
    offset
      | h < 1     = length $ catLinesBefore $ getBeforeLines e:bs
      | otherwise = boundOffset h $ min (paraLinesBefore da) k

paraLinesBefore :: EditingDocument -> Int
paraLinesBefore (EditingDocument bs e _ _ h _ _ _) = total where
  total = countLinesBefore h bs'
  bs' = getBeforeLines e:bs

paraLinesAfter :: EditingDocument -> Int
paraLinesAfter (EditingDocument _ e as _ h _ _ _) = total where
  total = countLinesAfter h as'
  as' = getAfterLines e:as

getVisibleLines :: EditingDocument -> [Text]
getVisibleLines (EditingDocument bs e as _ h k _ p) = visible where
  visible = map (renderLine p) $ bs2 ++ [e2] ++ as2
  bs2
    | h < 1     = reverse $ catLinesBefore before
    | otherwise = reverse $ take (boundOffset h k) $ catLinesBefore before
  e2 = getCurrentLine e
  as2
    | h < 1     = catLinesAfter after
    | otherwise = take (h-length bs2-1) $ catLinesAfter after
  before = getBeforeLines e:bs
  after = getAfterLines e:as

moveDocCursor :: MoveDirection -> EditingDocument -> EditingDocument
moveDocCursor d da@(EditingDocument bs e as w h k c p) = revised where
  revised
    | paraCursorMovable d e =
      let e2 = moveParaCursor d e in (EditingDocument bs e2 as w h (fixOffset e2) c p)
    | d == MoveDown && null as && atParaBack e =
      EditingDocument bs e as w h (boundOffset h (k-1)) c p
    | (d == MoveUp && null bs) || (d == MoveDown && null as) =
      -- NOTE: The cursor is explicitly stored here so that the position at the
      -- front or back of the paragraph is preserved.
      let e2 = moveParaCursor d e in storeCursor $ (EditingDocument bs e2 as w h (fixOffset e2) c p)
    | d == MoveUp =
        let
          bs2 = tail bs
          e2 = seekParaBack $ editPara p $ unparseParaBefore $ head bs
          as2 = viewParaAfter e:as
          k2 = boundOffset h (k-1) in
        -- NOTE: This doesn't preserve cursor position.
        EditingDocument bs2 e2 as2 w h k2 c p
    | d == MoveDown =
        let
          bs2 = viewParaBefore e:bs
          e2 = editPara p $ unparseParaAfter $ head as
          as2 = tail as
          k2 = boundOffset h (k+1) in
        -- NOTE: This doesn't preserve cursor position.
        EditingDocument bs2 e2 as2 w h k2 c p
    | d == MovePrev && not (null bs) = seekBack  $ moveDocCursor MoveUp   da
    | d == MoveNext && not (null as) = seekFront $ moveDocCursor MoveDown da
    | d == MovePageUp   = editAtTop $ repeatTimes h (moveDocCursor MoveUp)   da
    | d == MovePageDown = editAtTop $ repeatTimes h (moveDocCursor MoveDown) da
    | otherwise = da
  fixOffset e2 = boundOffset h $ k + (getParaCursorLine e2 - getParaCursorLine e)
  seekBack  (EditingDocument bs e as w h k c p) = EditingDocument bs (seekParaBack e)  as w h k c p
  seekFront (EditingDocument bs e as w h k c p) = EditingDocument bs (seekParaFront e) as w h k c p
  repeatTimes n = foldr (flip (.)) id . replicate n
  editAtTop (EditingDocument bs e as w h _ c p) = EditingDocument bs e as w h 0 c p

modifyDoc :: EditAction -> EditDirection -> EditingDocument -> EditingDocument
modifyDoc m d da@(EditingDocument bs e as w h k c p) = revised m d where
  revised DeleteText EditBefore
    | atParaFront e && not (null bs) =
      EditingDocument (tail bs) (prependToPara p (head bs) e) as w h (boundOffset h (k-1)) c p
  revised DeleteText EditAfter
    | atParaBack e && not (null as) =
      EditingDocument bs (appendToPara p e (head as)) (tail as) w h k c p
  revised _ _ = let e2 = (modifyPara p m d e) in EditingDocument bs e2 as w h (fixOffset e2) c p
  fixOffset e2 = boundOffset h $ k + (getParaCursorLine e2 - getParaCursorLine e)

insertParaSplit :: EditDirection -> EditingDocument -> EditingDocument
insertParaSplit d (EditingDocument bs e as w h k c p) = revised where
  (b,a) = splitPara p e
  revised
    | d == EditBefore = EditingDocument (parseParaBefore p b:bs) (editPara p a) as w h (boundOffset h (k+1)) c p
    | d == EditAfter  = EditingDocument bs (seekParaBack $ editPara p b) (parseParaAfter p a:as) w h k c p

getDocEditPoint :: EditingDocument -> (Int,Int)
getDocEditPoint (EditingDocument bs e _ _ _ _ _ _) = (length bs,getParaEditChar e)

setEditPara :: Int -> EditingDocument -> EditingDocument
setEditPara n (EditingDocument bs e as w h k c p) = revised where
  revised = resizeHeight h $ EditingDocument bs2 e2 as2 w h k c p
  (bs2,e2,as2) = shift bs e as
  shift bs e as
    | length bs < n && not (null as) =
      shift (viewParaBefore e:bs) (editPara p $ unparseParaAfter $ head as)  (tail as)
    | length bs > n && not (null bs) =
      shift (tail bs)             (editPara p $ unparseParaBefore $ head bs) (viewParaAfter e:as)
    | otherwise = (bs,e,as)

setEditChar :: Int -> EditingDocument -> EditingDocument
setEditChar n (EditingDocument bs e as w h k c p) = EditingDocument bs (setParaEditChar n e) as w h k c p

setDocEditPoint :: (Int,Int) -> EditingDocument -> EditingDocument
setDocEditPoint (p,c) = setEditChar c . setEditPara p

updateDocView :: ViewAction -> EditingDocument -> EditingDocument
updateDocView _ da@(EditingDocument _ _ _ _ h _ _ _) | h <= 0 = da
updateDocView FillView da@(EditingDocument bs e as w h k c p) = revised where
  revised = EditingDocument bs e as w h (boundOffset h (min before k2)) c p
  before = paraLinesBefore da
  after  = paraLinesAfter  da
  k2 = if after >= h-k-1  -- The view is already full.
          then k
          else h-after-1  -- k2+after=h-1, i.e., a full viewport.
updateDocView (ShiftVertical n) da@(EditingDocument bs e as w h k c p) = revised where
  revised = EditingDocument bs e as w h (boundOffset h k2) c p
  before = paraLinesBefore da
  k2 = min before (k-n)
