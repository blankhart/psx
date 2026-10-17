-- | The DOM normaliser. Two implementations of a scenario match when their
-- | DOMs print the same here, so what the printing ignores is what a
-- | framework may do differently:
-- |
-- | - how text is split into nodes (adjacent text is joined and empty text
-- |   dropped),
-- | - the order of attributes (they are sorted),
-- | - comments (the snapshot leaves them out), and
-- | - whether a form control's state was set as an attribute or a property
-- |   (the snapshot reports its live `value` and `checked`), and
-- | - `data-psx`, the attribute PSX stamps on the elements its scoped styles
-- |   root, which a hand-written `purs/` app has no reason to carry.
module Suite.Dom
  ( Node(..)
  , normalise
  , print
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..), fst)

data Node
  = Element String (Array (Tuple String String)) (Array Node)
  | Text String

normalise :: Array Node -> Array Node
normalise = Array.foldr step []
  where
  step node acc = case normaliseNode node, Array.uncons acc of
    Text "", _ -> acc
    Text a, Just { head: Text b, tail } -> Array.cons (Text (a <> b)) tail
    n, _ -> Array.cons n acc

  normaliseNode = case _ of
    Element tag attributes kids ->
      Element tag (Array.sortWith fst (Array.filter ((_ /= "data-psx") <<< fst) attributes)) (normalise kids)
    text -> text

-- | One element per line, indented by depth; each text node on a line of its
-- | own, quoted, so its exact whitespace shows.
print :: Array Node -> String
print = foldMap (printNode "")

printNode :: String -> Node -> String
printNode indent = case _ of
  Text s -> indent <> quote s <> "\n"
  Element tag attributes kids
    | Array.elem tag voidElements -> indent <> open tag attributes <> "\n"
    | Array.null kids -> indent <> open tag attributes <> "</" <> tag <> ">\n"
    | otherwise ->
        indent <> open tag attributes <> "\n"
          <> foldMap (printNode (indent <> "  ")) kids
          <> indent
          <> "</"
          <> tag
          <> ">\n"

open :: String -> Array (Tuple String String) -> String
open tag attributes = "<" <> tag <> foldMap attribute attributes <> ">"
  where
  attribute (Tuple name value)
    | value == "" = " " <> name
    | otherwise = " " <> name <> "=\"" <> escapeAttribute value <> "\""

escapeAttribute :: String -> String
escapeAttribute =
  String.replaceAll (String.Pattern "\"") (String.Replacement "&quot;")
    <<< String.replaceAll (String.Pattern "&") (String.Replacement "&amp;")

quote :: String -> String
quote s = "\"" <> foldMap escape (CodeUnits.toCharArray s) <> "\""
  where
  escape = case _ of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\t' -> "\\t"
    c -> CodeUnits.singleton c

voidElements :: Array String
voidElements =
  [ "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr" ]
