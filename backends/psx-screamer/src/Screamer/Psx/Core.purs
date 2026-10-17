-- | The hand-written core of the PSX backend for Screamer: the signature's
-- | names that are not one per element, attribute or event. `Screamer.Psx`
-- | re-exports it beside the per-name functions `psx gen-backend` writes
-- | from `backends/mappings/screamer.json`.
-- |
-- | Screamer binds signals to nodes and never re-renders, so its nodes have
-- | no message type: `Node` and `Attr` ignore their parameter, and the
-- | handlers run effects, so a view is `Markup (Effect Unit)`. Structure that
-- | changes is a signal in a hole: a `Signal String` is a text node updated
-- | in place, and a `Signal (Markup a)` (or `Signal (Node a)`) a `dyn`.
-- | Attributes take a constant or a signal of their type.
module Screamer.Psx.Core
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
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Screamer.Core as S
import Screamer.Signal (Signal)
import Web.Event.Event (Event)

type Node :: Type -> Type
type Node a = S.Element

type Attr :: Type -> Type
type Attr a = S.Attribute

-- | What every markup literal denotes.
type Markup a = Array (Node a)

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

text :: forall a. String -> Node a
text s = S.text (pure s)

element :: forall a. String -> Array (Attr a) -> Markup a -> Node a
element = S.element_

-- | A view runs once, so a literal's keyed children are created once and
-- | never reordered: the keys need nothing from Screamer. A list that changes
-- | is Screamer's own `each` in a hole.
keyedElement :: forall a. String -> Array (Attr a) -> Array (Tuple Key (Node a)) -> Node a
keyedElement tag attrs kids = S.element_ tag attrs (map snd kids)

keyed :: forall k a. ToKey k => k -> Node a -> Tuple Key (Node a)
keyed k node = Tuple (toKey k) node

attribute :: forall a. String -> String -> Attr a
attribute name value = S.attribute name (pure value)

-- | What a child hole may hold. Every instance is at `a = Effect Unit`, so a
-- | hole fixes a view's handlers to effects.
class Children c a | c -> a where
  children :: c -> Markup a

instance Children String (Effect Unit) where
  children s = [ text s ]

instance Children S.Element (Effect Unit) where
  children n = [ n ]

-- | A text node, updated in place.
instance Children (Signal String) (Effect Unit) where
  children s = [ S.text s ]

-- | A `dyn`: each new value replaces the nodes.
instance Children (Signal S.Element) (Effect Unit) where
  children s = [ S.dyn s ]

instance Children (Signal (Array S.Element)) (Effect Unit) where
  children s = [ S.dyn (S.fragment_ <$> s) ]

instance Children c a => Children (Array c) a where
  children = Array.concatMap children

instance Children c a => Children (Maybe c) a where
  children = maybe [] children

-- | What a child hole of a keyed parent may hold: keyed literals, or arrays
-- | of them.
class KeyedChildren c a | c -> a where
  keyedChildren :: c -> Array (Tuple Key (Node a))

instance KeyedChildren (Tuple String S.Element) (Effect Unit) where
  keyedChildren kid = [ kid ]

instance KeyedChildren c a => KeyedChildren (Array c) a where
  keyedChildren = Array.concatMap keyedChildren

instance KeyedChildren c a => KeyedChildren (Maybe c) a where
  keyedChildren = maybe [] keyedChildren

-- | The one element a view hands to `mount` or returns from `component`:
-- | the literal's only node, or a fragment of several.
root :: forall a. Markup a -> Node a
root nodes = case nodes of
  [ node ] -> node
  _ -> S.fragment_ nodes

-- | An `input` handler given the `value` of the element the event came
-- | from, or `""` if it has none.
onValueInput :: (String -> Effect Unit) -> Attr (Effect Unit)
onValueInput f = S.on_ "input" (f <=< targetValue)

foreign import targetValue :: Event -> Effect String
