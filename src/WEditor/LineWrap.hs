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

-- | Line-wrapping implementations. (See 'FixedFontParser' for custom wrapping.)

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module WEditor.LineWrap (
  BreakWords,
  LineBreak,
  WordSplitter(..),
  breakExact,
  breakWords,
  lazyHyphen,
  lineBreakEnd,
  lineBreakHyphen,
  lineBreakSimple,
  noHyphen,
  textWidth,
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified Data.Text as T
import qualified Graphics.Vty as V

import WEditor.Base


-- | Line break type for a single paragraph line.
data LineBreak = ParagraphEnd | SimpleBreak | HyphenatedWord deriving (Eq,Ord)

-- | The line is at the end of the paragraph.
lineBreakEnd :: LineBreak
lineBreakEnd = ParagraphEnd

-- | The line is nothing special.
lineBreakSimple :: LineBreak
lineBreakSimple = SimpleBreak

-- | The line ends with a hyphenated word.
lineBreakHyphen :: LineBreak
lineBreakHyphen = HyphenatedWord

data NoSplit = NoSplit deriving (Show)

-- | Wrapping policy that breaks at exactly the viewer width.
breakExact :: BreakWords
breakExact = breakWords NoSplit

textWidth :: Text -> Int
textWidth = V.wcswidth . T.unpack

splitColumns :: Int -> T.Text -> (T.Text, T.Text)
splitColumns w s = T.splitAt (fst $ T.foldl' f (0,0) s) s where
  -- The accumulator value is (index in Text value, width of Text so far)
  f (i,z) c
      -- Width was previously exceeded; continue with same values.
      | z < 0                   = (i, z)
      -- Width exceeded.  Signal this with z = -1.  Index will no longer be
      -- incremented.
      --
      -- Why not short circuit (e.g. using foldlM construction)?
      -- Because in the typical case, the Either allocation costs exceed
      -- any benefits.  The pathological case, string length >> width, is
      -- probably rare.
      | z + V.safeWcwidth c > w = (i, -1)
      -- Width not yet exceeded.  Increment index and add character width.
      | otherwise               = (i + 1, z + V.safeWcwidth c)

-- | Take up to the given width, having regard to character width.
takeColumns :: Int -> T.Text -> T.Text
takeColumns w = fst . splitColumns w

instance WordSplitter NoSplit

-- | Word-splitting operations for use with 'BreakWords'.
--
--     * @s@: Splitter type providing the operations.
--     * @c@: Character type.
class WordSplitter s where
  -- | Determine where to break a word.
  --
  --   * The splitter can refuse to process the word by returning 'Nothing'.
  --   * The segment sizes must provide space for a hyphen if 'appendHyphen'
  --     extends the line.
  --   * Once the word has been split up by 'BreakWords', the segments are
  --     processed as follows:
  --
  --       1. The last segment is prepended to the next line to be parsed. This
  --          means that if the word is not split, it gets deferred to the
  --          next line.
  --       2. If there are more segments, the first is appended to the current
  --          line being parsed.
  --       3. All remaining segments are put on separate lines between the
  --          current and next lines.
  splitWord :: s
            -> Int         -- ^ Space available on the first line.
            -> Int         -- ^ Space available on new lines.
            -> Text        -- ^ The word to break.
            -> Maybe [Int] -- ^ List of segment sizes.
  splitWord _ _ _ _ = Nothing
  -- | Predicate for characters that should be treated as a part of a word.
  isWordChar :: s -> Char -> Bool
  isWordChar _ _ = False
  -- | Predicate for detecting whitespace between words.
  isWhitespace :: s -> Char -> Bool
  isWhitespace _ _ = False
  -- | Append the canonical hyphen character to show word breaks.
  appendHyphen :: s -> Text -> Text
  appendHyphen _ = id
  -- | Check the word segment for an existing hyphenation.
  endsWithHyphen :: s -> Text -> Bool
  endsWithHyphen _ _ = False

-- | Wrapping policy that breaks lines based on words. Use 'breakWords' to
--   construct a new value.
data BreakWords = forall s. (Show s, WordSplitter s) => BreakWords Int s

-- | Wrapping policy that breaks lines based on words.
breakWords :: (Show s, WordSplitter s) => s -> BreakWords
breakWords = BreakWords 0

data NoHyphen = NoHyphen deriving (Show)

-- | Avoids splitting words unless they are longer than a single line.
noHyphen :: NoHyphen
noHyphen = NoHyphen

data LazyHyphen = LazyHyphen deriving (Show)

-- | Hyphenates words using simple aesthetics, without dictionary awareness.
lazyHyphen :: LazyHyphen
lazyHyphen = LazyHyphen

-- Private below here.

instance Show LineBreak where
  show ParagraphEnd   = "lineBreakEnd"
  show SimpleBreak    = "lineBreakSimple"
  show HyphenatedWord = "lineBreakHyphen"

instance Show BreakWords where
  show (BreakWords w s) =
    "breakWords { width: " ++ show w ++
               ", split: " ++ show s ++ " }"

instance WordSplitter NoHyphen where
  splitWord _ k w _ = if k < w then Just [] else Nothing
  isWordChar _ = defaultIsWordChar
  isWhitespace _ = defaultIsWhitespace

instance WordSplitter LazyHyphen where
  splitWord _ k w cs
    | w < 4 || k > w             = Nothing
    | k >= textWidth cs || k < 3 = Just []
    | otherwise = Just $ (k-1):(replicate count size) where
        size = w-1
        remainder = textWidth cs-(k-1)
        -- Uses remainder-2 because the last line needs no hyphen. This is the
        -- same as iteratively breaking off w-1 until the remainder is < w+1.
        count = (remainder-2) `div` size
  isWordChar _ = defaultIsWordChar
  isWhitespace _ = defaultIsWhitespace
  appendHyphen _ = (<> T.singleton defaultHyphen)
  endsWithHyphen _ cs = not (T.null cs) && isDefaultHyphen (T.last cs)

instance FixedFontParser BreakWords where
  type BreakType BreakWords = LineBreak
  setLineWidth (BreakWords _ s) w = BreakWords w s
  breakLines (BreakWords w s) = breakAllLines w s
  splitLine _ k (VisibleLine b cs) =
    (VisibleLine lineBreakEnd (T.take k cs),
     VisibleLine b            (T.drop k cs))
  emptyLine _ = VisibleLine lineBreakEnd mempty
  renderLine (BreakWords w _) (VisibleLine ParagraphEnd cs)
    | w < 1 = cs
    | otherwise = takeColumns w cs
  renderLine (BreakWords _ s) (VisibleLine SimpleBreak cs) =
    T.reverse $ T.dropWhile (isWhitespace s) $ T.reverse cs
  renderLine (BreakWords _ s) (VisibleLine HyphenatedWord cs) = appendHyphen s cs
  tweakCursor (BreakWords w _) (VisibleLine ParagraphEnd cs)
    | w < 1 = cursorPos cs
    | otherwise = min w . cursorPos cs
  tweakCursor (BreakWords _ s) (VisibleLine SimpleBreak cs) = max 0 . min (total-post) . cursorPos cs where
    post = textWidth $ T.takeWhile (isWhitespace s) $ T.reverse cs
    total = textWidth cs
  tweakCursor _ (VisibleLine HyphenatedWord _) = id

cursorPos :: Text -> Int -> Int
cursorPos s = textWidth . (`T.take` s)

breakAllLines :: WordSplitter s => Int -> s -> Text -> [VisibleLine LineBreak]
breakAllLines _ _ s | T.null s = [VisibleLine lineBreakEnd mempty]
breakAllLines w s cs
  | w < 1 = [VisibleLine lineBreakEnd cs]
  | otherwise = breakOrEmpty cs where
      breakOrEmpty cs' = ls where
        (ts, ds) = splitColumns w cs'
        (Just ls) = handleSplit (T.reverse ts) ds
      handleSplit line rest =
        tryWord line rest <|>
        trySpaces line rest <|>
        lineDefault line rest
      lineDefault ls  _ | T.null ls = Just []
      lineDefault ls rs | T.null rs = Just [VisibleLine lineBreakEnd (T.reverse ls)]
      lineDefault ls rs = Just $ VisibleLine lineBreakSimple (T.reverse ls):(breakOrEmpty rs)
      tryWord ls rs | not (T.null ls) && not (T.null rs) && isWordChar s l && isWordChar s r = newLines where
        l = T.head ls
        r = T.head rs
        newLines = do
          breaks <- splitWord s (textWidth wordFront) w word
          -- Safety fallback for misbehaving splitters.
          when (null breaks && textWidth wordFront == w) Nothing
          return $ case breaks of
                        []     -> VisibleLine lineBreakSimple (T.reverse ls2):(breakOrEmpty (word <> rs2))
                        (b:bs) -> (autoHyphen (T.reverse ls2 <> T.take b word)):(hyphenate (T.drop b word) bs)
        ls2 = T.dropWhile (isWordChar s) ls
        rs2 = T.dropWhile (isWordChar s) rs
        wordFront = T.reverse $ T.takeWhile (isWordChar s) ls
        wordBack = T.takeWhile (isWordChar s) rs
        word = wordFront <> wordBack
        autoHyphen ls = if endsWithHyphen s ls
                           then VisibleLine lineBreakSimple ls
                           else VisibleLine lineBreakHyphen ls
        hyphenate word bs | T.null word || null bs = breakOrEmpty (word <> rs2)
        hyphenate word (b:bs) = (autoHyphen (T.take b word)):(hyphenate (T.drop b word) bs)
      tryWord _ _ = Nothing
      trySpaces ls rs | not (T.null rs) && isWhitespace s (T.head rs) = newLines where
        ls' = T.reverse ls <> T.takeWhile (isWhitespace s) rs
        rs' = T.dropWhile (isWhitespace s) rs
        newLines
          | T.null rs' = Just [VisibleLine lineBreakEnd ls']
          | otherwise  = Just $ (VisibleLine lineBreakSimple ls'):(breakOrEmpty rs')
      trySpaces _ _ = Nothing
