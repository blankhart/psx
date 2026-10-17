-- | The hand-written core of the PSX backend for Portico: the signature's
-- | names that are not one per element, attribute or event. `Portico.Psx`
-- | re-exports it beside the per-name functions `psx gen-backend` writes
-- | from `backends/mappings/portico.json`.
module Portico.Psx.Core
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
  , booleanAttribute
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..), snd)
import Portico as P
import Unsafe.Coerce (unsafeCoerce)

type Node = P.Html

type Attr = P.Trait

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
text = P.text

element :: forall a. String -> Array (Attr a) -> Markup a -> Node a
element = P.element

-- | Portico keys a list through its children, so the keys are already on
-- | the nodes (see `keyed`) and the parent is an ordinary element.
keyedElement :: forall a. String -> Array (Attr a) -> Array (Tuple Key (Node a)) -> Node a
keyedElement tag attrs kids = P.element tag attrs (map snd kids)

-- | The node with the key added to its traits, as `P.key` would have put it
-- | there.
keyed :: forall k a. ToKey k => k -> Node a -> Tuple Key (Node a)
keyed k node = Tuple (toKey k) (runFn2 withKey_ (P.key (toKey k)) node)

foreign import withKey_ :: forall a. Fn2 (Attr a) (Node a) (Node a)

attribute :: forall a. String -> String -> Attr a
attribute = P.attribute

-- | What a child hole may hold. The dependency lets a hole's nodes fix the
-- | message type, as an annotation would.
class Children c a | c -> a where
  children :: c -> Markup a

instance Children String a where
  children s = [ P.text s ]

instance Children (P.Html a) a where
  children n = [ n ]

instance Children c a => Children (Array c) a where
  children = Array.concatMap children

instance Children c a => Children (Maybe c) a where
  children = maybe [] children

-- | What a child hole of a keyed parent may hold: keyed literals, or arrays
-- | of them.
class KeyedChildren c a | c -> a where
  keyedChildren :: c -> Array (Tuple Key (Node a))

instance KeyedChildren (Tuple String (P.Html a)) a where
  keyedChildren kid = [ kid ]

instance KeyedChildren c a => KeyedChildren (Array c) a where
  keyedChildren = Array.concatMap keyedChildren

instance KeyedChildren c a => KeyedChildren (Maybe c) a where
  keyedChildren = maybe [] keyedChildren

-- | The one node a view hands to `P.mount`: the literal's only node, or a
-- | `div` around several.
root :: forall a. Markup a -> Node a
root nodes = case nodes of
  [ node ] -> node
  _ -> P.element "div" [] nodes

-- | A boolean attribute: present when true, absent when false. snabbdom's
-- | attributes module reads `true` and `false` that way.
booleanAttribute :: forall a. String -> Boolean -> Attr a
booleanAttribute name present = P.attribute name (unsafeCoerce present)
