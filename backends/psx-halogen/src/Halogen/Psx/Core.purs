-- | The hand-written core of the PSX backend for Halogen: the signature's
-- | names that are not one per element, attribute or event. `Halogen.Psx`
-- | re-exports it beside the per-name functions `psx gen-backend` writes
-- | from `backends/mappings/halogen.json`.
-- |
-- | Halogen's nodes carry the slot type `w` beside the action type, and its
-- | properties the row of the element they belong to, so `Node`, `Markup`
-- | and `Attr` each take one more parameter than the signature's: a view is
-- | `Markup w Action`, and `HH.ComponentHTML Action slots m` is a `Node`.
module Halogen.Psx.Core
  ( Node
  , Attr
  , Markup
  , Key
  , class ToKey
  , toKey
  , text
  , element
  , keyedElement
  , keyed
  , attribute
  , class Children
  , children
  , class KeyedChildren
  , keyedChildren
  , root
  , onValueInput
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (Event, EventType(..))

type Node = HH.HTML

-- | A property of an element whose row is `r`.
type Attr = HH.IProp

-- | What every markup literal denotes.
type Markup w a = Array (Node w a)

type Key = String

class ToKey k where
  toKey :: k -> Key

instance ToKey String where
  toKey = identity

instance ToKey Int where
  toKey = show

instance ToKey Number where
  toKey = show

instance ToKey Char where
  toKey = show

text :: forall w a. String -> Node w a
text = HH.text

element :: forall r w a. String -> Array (Attr r a) -> Markup w a -> Node w a
element tag = HH.element (HH.ElemName tag)

-- | Halogen's own keyed element, which halogen-vdom diffs by key.
keyedElement :: forall r w a. String -> Array (Attr r a) -> Array (Tuple Key (Node w a)) -> Node w a
keyedElement tag = HH.keyed (HH.ElemName tag)

keyed :: forall k w a. ToKey k => k -> Node w a -> Tuple Key (Node w a)
keyed k node = Tuple (toKey k) node

attribute :: forall r a. String -> String -> Attr r a
attribute name = HH.attr (HH.AttrName name)

-- | What a child hole may hold. The dependency lets a hole's nodes fix the
-- | slot and action types, as an annotation would.
class Children c w a | c -> w a where
  children :: c -> Markup w a

instance Children String w a where
  children s = [ HH.text s ]

instance Children (HH.HTML w a) w a where
  children n = [ n ]

instance Children c w a => Children (Array c) w a where
  children = Array.concatMap children

instance Children c w a => Children (Maybe c) w a where
  children = maybe [] children

-- | What a child hole of a keyed parent may hold: keyed literals, or arrays
-- | of them.
class KeyedChildren c w a | c -> w a where
  keyedChildren :: c -> Array (Tuple Key (Node w a))

instance KeyedChildren (Tuple String (HH.HTML w a)) w a where
  keyedChildren kid = [ kid ]

instance KeyedChildren c w a => KeyedChildren (Array c) w a where
  keyedChildren = Array.concatMap keyedChildren

instance KeyedChildren c w a => KeyedChildren (Maybe c) w a where
  keyedChildren = maybe [] keyedChildren

-- | The one node a `render` returns: the literal's only node, or a `div`
-- | around several.
root :: forall w a. Markup w a -> Node w a
root nodes = case nodes of
  [ node ] -> node
  _ -> HH.div [] nodes

-- | An `input` handler given the `value` of the element the event came from,
-- | or `""` if it has none.
onValueInput :: forall r a. (String -> a) -> Attr (onInput :: Event | r) a
onValueInput f = HE.handler (EventType "input") (f <<< targetValue)

foreign import targetValue :: Event -> String
