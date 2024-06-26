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

-- | Generic text-editing functionality.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

module WEditor.Base.Editor (
  EditAction(..),
  EditDirection(..),
  EditorAction,
  MoveDirection(..),
  FixedFontEditor(..),
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
) where

import WEditor.Base.Para


-- | Generic text editor for fixed-width fonts.
--
--     * @e@: Editor type providing the operations.
--     * @c@: Character type.
class FixedFontEditor e where
  -- | Apply an edit action.
  editText :: e -> EditAction -> EditDirection -> e
  -- | Break the current paragraph at the cursor.
  breakPara :: e -> EditDirection -> e
  -- | Apply e cursor movement.
  moveCursor :: e -> MoveDirection -> e
  -- | Get the @(col,row)@ cursor position relative to the viewport.
  getCursor :: e -> (Int,Int)
  -- | Get the absolute @(paragraph,char)@ edit position.
  --
  --   The position can be restored after cursor movements by calling
  --   'setEditPoint'; however, calling 'editText' or 'breakPara' invalidates
  --   this position.
  getEditPoint :: e -> (Int,Int)
  -- | Set the absolute @(paragraph,char)@ edit position.
  setEditPoint :: e -> (Int,Int) -> e
  -- | Get the number of characters in the current paragraph.
  getParaSize :: e -> Int
  -- | Get the number of paragraphs in the document.
  getParaCount :: e -> Int
  -- | Export the modified data.
  exportData :: e -> [UnparsedPara]

-- | Actions that modify data.
data EditAction =
  InsertText Text | -- ^ Insert a block of characters.
  DeleteText       -- ^ Delete a single character.
    deriving (Eq,Show)

-- | Modification direction, relative to the cursor.
data EditDirection =
  EditBefore | -- ^ Apply the edit before the cursor.
  EditAfter    -- ^ Apply the edit after the cursor.
    deriving (Eq,Show)

-- | Actions that change the cursor position without changing data.
data MoveDirection =
  MoveUp |     -- ^ Move up one line.
  MoveDown |   -- ^ Move down one line.
  MovePrev |   -- ^ Move backward one character.
  MoveNext |   -- ^ Move forward one character.
  MoveHome |   -- ^ Implementation-defined home operation.
  MoveEnd |    -- ^ Implementation-defined end operation.
  MovePageUp | -- ^ Implementation-defined page-up operation.
  MovePageDown -- ^ Implementation-defined page-down operation.
  deriving (Eq,Show)

-- | Any action that updates a 'FixedFontEditor'.
type EditorAction = forall e. FixedFontEditor e => e -> e

-- | Action for the @Backspace@ key.
editorBackspaceAction :: EditorAction
editorBackspaceAction e = editText e DeleteText EditBefore

-- | Action for the @Delete@ key.
editorDeleteAction :: EditorAction
editorDeleteAction e = editText e DeleteText EditAfter

-- | Action for the @Enter@ key.
editorEnterAction :: EditorAction
editorEnterAction e = breakPara e EditBefore

-- | Action for normal character insertion.
editorAppendAction :: Text -> EditorAction
editorAppendAction cs e = editText e (InsertText cs) EditBefore

-- | Action to insert after the cursor.
editorInsertAction :: Text -> EditorAction
editorInsertAction cs e = editText e (InsertText cs) EditAfter

-- | Action to set the absolute (paragraph,char) edit position.
editorSetPositionAction :: (Int,Int) -> EditorAction
editorSetPositionAction (p,c) e = setEditPoint e (p,c)

-- | Action for the up-arrow key.
editorUpAction :: EditorAction
editorUpAction e = moveCursor e MoveUp

-- | Action for the down-arrow key.
editorDownAction :: EditorAction
editorDownAction e = moveCursor e MoveDown

-- | Action for the left-arrow key.
editorLeftAction :: EditorAction
editorLeftAction e = moveCursor e MovePrev

-- | Action for the right-arrow key.
editorRightAction :: EditorAction
editorRightAction e = moveCursor e MoveNext

-- | Action for the @Home@ key.
editorHomeAction :: EditorAction
editorHomeAction e = moveCursor e MoveHome

-- | Action for the @End@ key.
editorEndAction :: EditorAction
editorEndAction e = moveCursor e MoveEnd

-- | Action for the @PageUp@ key.
editorPageUpAction :: EditorAction
editorPageUpAction e = moveCursor e MovePageUp

-- | Action for the @PageDown@ key.
editorPageDownAction :: EditorAction
editorPageDownAction e = moveCursor e MovePageDown
