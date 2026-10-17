module Test.Portico.Psx where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Portico.Psx as B

main :: Effect Unit
main = do
  keys
  roots
  log "psx-portico: all tests passed"

assert :: String -> Boolean -> Effect Unit
assert what ok = unless ok (throw ("FAIL " <> what))

-- Portico's nodes as the JavaScript arrays they are (see Portico.Foreign).
foreign import describe :: forall a. B.Node a -> String

-- `keyed` puts Portico's key trait on the element, also below `map`.
keys :: Effect Unit
keys = do
  let Tuple k node = B.keyed 7 (B.li [ B.class_ "row" ] [ B.text "seven" ])
  assert "the key is the ToKey string" (k == "7")
  assert ("the element carries the key: " <> describe node) (describe node == "li[property className=row, key 7](seven)")
  let Tuple _ mapped = B.keyed "m" (map (_ + 1) (B.li [] [] :: B.Node Int))
  assert ("the key goes below map: " <> describe mapped) (describe mapped == "map(li[key m]())")
  let parent = B.keyedElement "ul" [] [ B.keyed "a" (B.li [] []), B.keyed "b" (B.li [] []) ] :: B.Node Unit
  assert ("a keyed element is an element of keyed nodes: " <> describe parent) (describe parent == "ul[](li[key a](), li[key b]())")

-- `root` is the only node, or a div around several.
roots :: Effect Unit
roots = do
  let one = B.root [ B.p [] [] ] :: B.Node Unit
  assert ("one node is itself: " <> describe one) (describe one == "p[]()")
  let several = B.root (B.children "a" <> B.children [ B.p [] [] ]) :: B.Node Unit
  assert ("several are wrapped: " <> describe several) (describe several == "div[](a, p[]())")

