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

-- | Simple representation of text paragraphs.

{-# LANGUAGE Safe #-}

module WEditor.Base.Para (
  Text,
  UnparsedPara(..),
  emptyPara,
) where

import Data.Text (Text)


-- | Single paragraph that has not been parsed into lines.
newtype UnparsedPara =
  UnparsedPara {
    upText :: Text -- ^ The complete and uparsed paragraph data.
  } deriving (Eq,Ord,Show)

-- | Create an empty paragraph.
emptyPara :: UnparsedPara
emptyPara = UnparsedPara mempty
