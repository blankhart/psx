-- | A scenario's DOM contracts: the normalised DOM after a step, committed
-- | as `test/suite/scenarios/<scenario>/<name>.html`.
-- |
-- | A DOM longer than `maxLines` lines, such as the table scenario's
-- | thousands of rows, is committed as its first `headLines` lines and a
-- | last line with the number of lines left and their SHA-256. The head can
-- | be reviewed in a diff and the rest is still checked exactly.
module Suite.Contract
  ( committed
  , diff
  , sha256
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String as String

maxLines :: Int
maxLines = 400

headLines :: Int
headLines = 200

-- | What the contract file holds for this printed DOM.
committed :: String -> String
committed printed =
  if Array.length lines <= maxLines then printed
  else
    String.joinWith "" (map (_ <> "\n") (Array.take headLines lines))
      <> "… "
      <> show (Array.length rest)
      <> " more lines, sha256 "
      <> sha256 (String.joinWith "" (map (_ <> "\n") rest))
      <> "\n"
  where
  -- `printed` ends with a newline, so the last piece is empty.
  lines = fromMaybe [] (Array.init (String.split (String.Pattern "\n") printed))
  rest = Array.drop headLines lines

-- | The first line that differs, with the lines around it.
diff :: String -> String -> String
diff expected actual =
  let
    es = String.split (String.Pattern "\n") expected
    as = String.split (String.Pattern "\n") actual
    at = fromMaybe (Array.length es) $ Array.findIndex identity
      $ Array.zipWith (/=) (es <> [ "" ]) (as <> [ "" ])
    window xs = Array.slice (max 0 (at - 3)) (at + 4) xs
  in
    "expected:\n" <> String.joinWith "\n" (map ("  " <> _) (window es))
      <> "\nactual:\n"
      <> String.joinWith "\n" (map ("  " <> _) (window as))

-- | Hex SHA-256 of the string's UTF-8 bytes.
foreign import sha256 :: String -> String
