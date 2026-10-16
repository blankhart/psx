-- | The whitespace policy for text in markup: on every line but the first,
-- | leading whitespace is trimmed; on every line but the last, trailing
-- | whitespace is trimmed; empty lines are dropped; the lines that remain
-- | are joined with one space.
-- |
-- | So whitespace within a line is kept as written (`{a}: {b}` keeps ": "),
-- | and text that is only indentation between tags disappears.
module Psx.Lower.Whitespace
  ( normalise
  , isWhitespace
  , trimStart
  , trimEnd
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as SCU

normalise :: String -> String
normalise text = do
  let
    lines = map stripCR (String.split (String.Pattern "\n") text)
    lastIndex = Array.length lines - 1
    trimmed = lines # Array.mapWithIndex \i line -> do
      let line' = if i > 0 then trimStart line else line
      if i < lastIndex then trimEnd line' else line'
  String.joinWith " " (Array.filter (_ /= "") trimmed)
  where
  stripCR line = if SCU.takeRight 1 line == "\r" then SCU.dropRight 1 line else line

-- | HTML's whitespace: space, tab, line feed, form feed and carriage return.
-- | A non-breaking space is text.
isWhitespace :: Char -> Boolean
isWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\x0C' || c == '\r'

trimStart :: String -> String
trimStart s = SCU.drop (SCU.length (SCU.takeWhile isWhitespace s)) s

trimEnd :: String -> String
trimEnd s = go (SCU.length s)
  where
  go n = case SCU.charAt (n - 1) s of
    Just c | isWhitespace c -> go (n - 1)
    _ -> SCU.take n s
