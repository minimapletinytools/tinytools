-- WIP
module Potato.Data.Text.Zipper2 where

import           Prelude

import Control.Exception (assert)
import Control.Monad.State (evalState, forM, get, put)
import Data.Char (isSpace)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import Data.Text.Internal (Text(..), text)
import Data.Text.Internal.Fusion (stream)
import Data.Text.Internal.Fusion.Types (Stream(..), Step(..))
import Data.Text.Unsafe
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T

import Graphics.Text.Width (wcwidth)

import qualified Data.List.NonEmpty as NE

-- | Get the display width of a 'Char'. "Full width" and "wide" characters
-- take two columns and everything else takes a single column. See
-- <https://www.unicode.org/reports/tr11/> for more information
-- This is implemented using wcwidth from Vty such that it matches what will
-- be displayed on the terminal. Note that this method can change depending
-- on how vty is configed. Please see vty documentation for details.
charWidth :: Char -> Int
charWidth = wcwidth

-- TERMINOLOGY
-- selection: the portion of the TextZipper that is selected
-- cursor: the cursor is defined as the begining and end of the selection
-- word: a word is defined as a contiguous set of non-whitespace characters in the TextZipper
--   the position one word to the left/right of the cursor is the position after all non-white space to the left/right of the cursor until it hits non-whitespace character followde by all contiguous non-whitespace characters in that direction
-- logical lines: logical lines of the TextZipper are lines created by explicit new line characters
-- display lines: display lines of a TextZipper ar the lines rendered to screen
--   display lines are bound by some width
--

data TextZipper = TextZipper
  { _textZipper_linesBefore :: [Text] -- reversed
  , _textZipper_before :: Text
  , _textZipper_selected :: [Text]
  , _textZipper_after :: Text
  , _textZipper_linesAfter :: [Text]
  }
  deriving (Show, Eq)

-- example:
--
-- this is an example content of
-- a text zipper
-- the capital TEXT IS THE SELECTED
-- PORTION of the
-- text zipper
--
-- _textZipper_linesBefore = ["this is an example content of", "a text zipper"]
-- _textZipper_before = "the capital "
-- _textZipper_selected = ["TEXT IS THE SELECTED", "PORTION"]
-- _textZipper_after = " of the"
-- _textZipper_linesAfter = ["text zipper"]

instance IsString TextZipper where
  fromString = fromText . T.pack

-- | Map a replacement function over the characters in a 'TextZipper'
mapZipper :: (Char -> Char) -> TextZipper -> TextZipper
mapZipper f (TextZipper lb b s a la) = TextZipper
  { _textZipper_linesBefore = fmap (T.map f) lb
  , _textZipper_before = T.map f b
  , _textZipper_selected = fmap (T.map f) s
  , _textZipper_after = T.map f a
  , _textZipper_linesAfter = fmap (T.map f) la
  }

appendEnd :: [Text] -> Text -> [Text]
appendEnd stuff addme = case stuff of
  [] -> [addme]
  (x:[]) -> [x <> addme]
  (x:xs) -> x : appendEnd xs addme

-- | Move the cursor left one character (clearing the selection)
left :: TextZipper -> TextZipper
left = leftN 1

-- UNTESTED
-- | Move the cursor left by the given number of characters (clearing the selection)
leftN :: Int -> TextZipper -> TextZipper
leftN n z@(TextZipper lb b [] a la) =
  if T.length b >= n
    then
      let n' = T.length b - n
      in  TextZipper lb (T.take n' b) [] (T.drop n' b <> a) la
    else case lb of
           [] -> home z
           (l:ls) -> leftN (n - T.length b - 1) $ TextZipper ls l [] "" ((b <> a) : la)
leftN n (TextZipper lb b s a la) = leftN n $ TextZipper lb b [] newa newla  where
  (newa, newla') = case s of
    [] -> (a, la)
    (x:[]) -> (x <> a, la)
    (x:xs) -> (x, appendEnd xs a)
  newla = newla' <> la


-- | expand the selection to the left the given number of characters
shiftLeftN :: TextZipper -> TextZipper
shiftLeftN = undefined

-- | Move the cursor to the left one word (clearing the selection)
leftWord :: TextZipper -> TextZipper
leftWord = undefined

-- | Expand the selection to the left by one word
shiftLeftWord :: TextZipper -> TextZipper
shiftLeftWord = undefined


-- | Move the cursor right one character (clearing the selection)
right :: TextZipper -> TextZipper
right = rightN 1

-- | Move the character right by the given number of characters (clearing the selection)
rightN :: Int -> TextZipper -> TextZipper
rightN n z@(TextZipper lb b s a la) = undefined

-- | expand the selection to the right the given number of characters
shiftRightN :: TextZipper -> TextZipper
shiftRightN = undefined

-- | Move the cursor to the right one word (clearing the selection)
rightWord :: TextZipper -> TextZipper
rightWord = undefined

-- | Expand the selection to the right by one word
rightLeftWord :: TextZipper -> TextZipper
rightLeftWord = undefined

-- | Clear the selection and move the cursor to the end of selection
deselect :: TextZipper -> TextZipper
deselect tz@(TextZipper lb b []           a la) = tz
deselect    (TextZipper lb b [x]          a la) = TextZipper lb (b <> x) [] a la
deselect    (TextZipper lb b (x:(xs:xss)) a la) = TextZipper ((reverse $ NE.init xs') <> [b <> x] <> lb) (NE.last xs') [] a la where
                xs' = xs NE.:| xss

-- | Move the cursor up one logical line (clearing the selection)
up :: TextZipper -> TextZipper
up    (TextZipper []     b [] a la) = TextZipper [] "" [] (b <> a) la
up    (TextZipper (x:xs) b [] a la) = TextZipper (NE.tail lb) b' [] a' ((b <> a):la) where
        lb = x NE.:| xs
        (b', a') = T.splitAt (T.length b) x
up    (TextZipper [] b [y] a la) = TextZipper [] "" [] (b <> y <> a) la
up    (TextZipper lb b (y:(ys:yss)) a la) = TextZipper lb "" [] (b <> y) (NE.init ys' <> [NE.last ys' <> a] <> la) where
        ys' = ys NE.:| yss
up    (TextZipper (x:xs) b [y] a la) = TextZipper (NE.tail lb) b' [] a' ((by <> a):la) where
        lb = x NE.:| xs
        by = b <> y
        (b', a') = T.splitAt (T.length by) x

-- | Move the cursor down one logical line (clearing the selection)
down :: TextZipper -> TextZipper
down (TextZipper lb b [] a [])     = TextZipper lb (b <> a) [] "" []
down (TextZipper lb b [] a (x:xs)) = TextZipper ((b <> a):lb) b' [] a' xs where
        (b', a') = T.splitAt (T.length b) x
down (TextZipper lb b [x] a [])     = TextZipper lb (b <> x <> a) [] "" []
down (TextZipper lb b (x:(xs:xss)) a []) = TextZipper (NE.init xs' <> (b:x:lb)) (NE.last xs' <> a) [] "" [] where
        xs' = xs NE.:| xss
down (TextZipper lb b [y] a (x:xs)) = TextZipper ((by <> a):lb) b' [] a' xs where
        by = b <> y
        (b', a') = T.splitAt (T.length by) x
down (TextZipper lb b (y:(ys:yss)) a (x:xs)) = TextZipper ((reverse $ NE.init ys') <>  [b <> y] <> lb) b' [] a'  xs where
        ys' = ys NE.:| yss
        (b', a') = T.splitAt (T.length $ NE.last ys') x
        
-- | Move the cursor up by the given number of lines (clearing the selection)
pageUp :: Int -> TextZipper -> TextZipper
pageUp pageSize z = undefined

-- | Move the cursor down by the given number of lines (clearing the selection)
pageDown :: Int -> TextZipper -> TextZipper
pageDown pageSize z = undefined

-- | Move the cursor to the beginning of the current logical line (clearing the selection)
home :: TextZipper -> TextZipper
home (TextZipper lb b [] a la) = TextZipper lb "" [] (b <> a) la
home (TextZipper lb b (x:[]) a la) = TextZipper lb "" [] (b <> x <> a) la
home (TextZipper lb b (x:(xs:xss)) a la) = TextZipper lb "" [] (b <> x) (NE.init xs' <> [NE.last xs' <> a] <> la) where
   xs' = xs NE.:| xss

-- | Move the cursor to the end of the current logical line (clearing the selection)
end :: TextZipper -> TextZipper
end (TextZipper lb b [] a la) = TextZipper lb (b <> a) [] "" la
end (TextZipper lb b (x:[]) a la) = TextZipper lb (b <> x <> a) [] "" la
end (TextZipper lb b (x:(xs:xss)) a la) =TextZipper (lb <> ([b <> x] <> NE.init xs')) (NE.last xs' <> a) [] "" la where
   xs' = xs NE.:| xss

-- | Move the cursor to the top of the document (clearing the selection)
top :: TextZipper -> TextZipper
top tz@(TextZipper [] "" [] a la) = tz
top (TextZipper [x]    "" [] a la) = TextZipper [] "" [] x (a:la)
top (TextZipper (x:xs) "" [] a la) = TextZipper [] "" [] (NE.last xs') ((reverse $ NE.init xs') <> (a:la)) where
    xs' = x NE.:| xs
top tz = top $ home tz

-- | Insert a character at the current cursor position (overwriting the selection)
insertChar :: Char -> TextZipper -> TextZipper
insertChar i = insert (T.singleton i)

-- | Insert text at the current cursor position (overwriting the selection)
insert :: Text -> TextZipper -> TextZipper
insert i z@(TextZipper lb b s a la) = case T.split (=='\n') i of
  [] -> z
  (x:xs) -> undefined

-- | Delete the selection
deleteSelection :: TextZipper -> TextZipper
deleteSelection = undefined

-- | Delete the selection or the character to the left of the cursor if there was no selection
deleteLeft :: TextZipper-> TextZipper
deleteLeft z@(TextZipper lb b s a la) = undefined

-- | Delete the selection to the character to the right of the cursor if there was no selection
deleteRight :: TextZipper -> TextZipper
deleteRight z@(TextZipper lb b s a la) = undefined

-- | Delete the selection and the word to the left of the cursor and the selection.
-- When deleting the word to the left of the selection, deletes all whitespace until it finds a non-whitespace character, and then deletes contiguous non-whitespace characters.
deleteLeftWord :: TextZipper -> TextZipper
deleteLeftWord (TextZipper lb b s a la) = undefined

-- | Insert up to n spaces to get to the next logical column that is a multiple of n
tab :: Int -> TextZipper -> TextZipper
tab n (TextZipper _ b s _ _) = undefined

-- | The plain text contents of the zipper
value :: TextZipper -> Text
value (TextZipper lb b s a la) = undefined


-- | The empty zipper
empty :: TextZipper
empty = TextZipper [] "" [] "" []

-- | Constructs a zipper with the given contents. The cursor is placed after the contents.
fromText :: Text -> TextZipper
fromText = flip insert empty


-- | Text alignment type
data TextAlignment =
  TextAlignment_Left
  | TextAlignment_Right
  | TextAlignment_Center
  deriving (Eq, Show)



-- A map from the row index of display line to a tuple (fst,snd) where
--   fst: leading empty spaces from left (may be negative) to adjust for alignment
--   snd: the text offset from the beginning of the document
type OffsetMapWithAlignment = Map Int (Int, Int)


-- | Information about the document as it is displayed (i.e., post-wrapping)
data DisplayLines = DisplayLines
  { _displayLines_text :: [[Text]] -- outer is logical lines, inner list is display lines created due to wrapping logical lines to display width
  , _displayLines_offsetMap :: OffsetMapWithAlignment -- note that the row index (key) of OffsetMapWithAlignment counts display lines which includes logical lines
  , _displayLines_cursorPos :: (Int, Int) -- cursor position relative to upper left hand corner
  , _displayLines_selectionCount :: Int
  }
  deriving (Eq, Show)

-- | Adjust the cursor and/or selection of the 'TextZipper' by the given display line coordinates
-- If the x coordinate is beyond the start/end of a line, the cursor is moved to the start/end of that line respectively
-- if `add` is true, the selection is expanded to the given position
-- if `add` is false, the selection is cleared and the cursor is moved to the given position
goToDisplayLinePosition :: Bool -> Int -> Int -> DisplayLines -> TextZipper -> TextZipper
goToDisplayLinePosition add x y dl tz = undefined
-- | Given a `TextAlignment`, a width and a 'TextZipper', produce a `DisplayLines`
-- wrapping happens at word boundaries such that the most possible words fit into each display line
-- if a line can not be wrapped (i.e. it contains a word longer than the display width) then the line is cropped in the middle of the word as necessary
displayLinesWithAlignment
  :: TextAlignment
  -> Int -- ^ Width, used for wrapping
  -> TextZipper -- ^ The text input contents and cursor state
  -> DisplayLines
displayLinesWithAlignment = undefined
