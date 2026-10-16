-- | Character references in markup text and quoted attribute values:
-- | `&#38;`, `&#x26;` and the named references in common use. A `&` that
-- | starts no reference it knows is left as written.
module Psx.Lower.Entities
  ( decode
  ) where

import Prelude

import Data.Array as Array
import Data.Enum (toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

decode :: String -> String
decode s = case SCU.indexOf (String.Pattern "&") s of
  Nothing -> s
  Just i -> do
    let
      before = SCU.take i s
      rest = SCU.drop i s
    case reference rest of
      Just { text, length } -> before <> text <> decode (SCU.drop length rest)
      Nothing -> before <> "&" <> decode (SCU.drop 1 rest)

-- | The reference at the start of a string that begins with `&`, and how many
-- | code units it spans.
reference :: String -> Maybe { text :: String, length :: Int }
reference s = do
  semi <- SCU.indexOf (String.Pattern ";") s
  let body = SCU.slice 1 semi s
  text <- case SCU.take 1 body of
    "#" -> do
      let digits = SCU.drop 1 body
      code <- case SCU.take 1 digits of
        "x" -> Int.fromStringAs Int.hexadecimal (SCU.drop 1 digits)
        "X" -> Int.fromStringAs Int.hexadecimal (SCU.drop 1 digits)
        _ -> if digits /= "" && Array.all isDigit (SCU.toCharArray digits) then Int.fromString digits else Nothing
      codePoint code
    _ -> Object.lookup body named
  pure { text, length: semi + 1 }
  where
  isDigit c = c >= '0' && c <= '9'

codePoint :: Int -> Maybe String
codePoint n
  | n >= 0xD800 && n <= 0xDFFF = Nothing
  | n <= 0 = Nothing
  | otherwise = String.singleton <$> toEnum n

named :: Object String
named = Object.fromFoldable $ map (\(Tuple k v) -> Tuple k (fromMaybe "" (codePoint v)))
  [ Tuple "amp" 38, Tuple "lt" 60, Tuple "gt" 62, Tuple "quot" 34, Tuple "apos" 39
  , Tuple "nbsp" 160, Tuple "iexcl" 161, Tuple "cent" 162, Tuple "pound" 163, Tuple "curren" 164
  , Tuple "yen" 165, Tuple "brvbar" 166, Tuple "sect" 167, Tuple "uml" 168, Tuple "copy" 169
  , Tuple "ordf" 170, Tuple "laquo" 171, Tuple "not" 172, Tuple "shy" 173, Tuple "reg" 174
  , Tuple "macr" 175, Tuple "deg" 176, Tuple "plusmn" 177, Tuple "sup2" 178, Tuple "sup3" 179
  , Tuple "acute" 180, Tuple "micro" 181, Tuple "para" 182, Tuple "middot" 183, Tuple "cedil" 184
  , Tuple "sup1" 185, Tuple "ordm" 186, Tuple "raquo" 187, Tuple "frac14" 188, Tuple "frac12" 189
  , Tuple "frac34" 190, Tuple "iquest" 191, Tuple "times" 215, Tuple "divide" 247
  , Tuple "ndash" 8211, Tuple "mdash" 8212, Tuple "lsquo" 8216, Tuple "rsquo" 8217
  , Tuple "sbquo" 8218, Tuple "ldquo" 8220, Tuple "rdquo" 8221, Tuple "bdquo" 8222
  , Tuple "dagger" 8224, Tuple "Dagger" 8225, Tuple "bull" 8226, Tuple "hellip" 8230
  , Tuple "permil" 8240, Tuple "prime" 8242, Tuple "Prime" 8243, Tuple "lsaquo" 8249
  , Tuple "rsaquo" 8250, Tuple "euro" 8364, Tuple "trade" 8482, Tuple "larr" 8592
  , Tuple "uarr" 8593, Tuple "rarr" 8594, Tuple "darr" 8595, Tuple "harr" 8596
  , Tuple "lArr" 8656, Tuple "uArr" 8657, Tuple "rArr" 8658, Tuple "dArr" 8659, Tuple "hArr" 8660
  , Tuple "minus" 8722, Tuple "le" 8804, Tuple "ge" 8805, Tuple "ne" 8800, Tuple "asymp" 8776
  , Tuple "infin" 8734, Tuple "ensp" 8194, Tuple "emsp" 8195, Tuple "thinsp" 8201
  , Tuple "zwnj" 8204, Tuple "zwj" 8205, Tuple "lrm" 8206, Tuple "rlm" 8207
  , Tuple "check" 10003, Tuple "cross" 10007, Tuple "star" 9734, Tuple "starf" 9733
  ]
