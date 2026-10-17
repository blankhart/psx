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
  , svgElement
  , class Content
  , content
  , elementOf
  , none
  , keyed
  , attribute
  , class Children
  , children
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

element :: forall c a. Content c a => String -> Array (Attr a) -> c -> Node a
element tag = elementOf (P.element tag)

-- | An SVG element. Portico sets `class` as the `className` property, which
-- | does nothing on SVG, so here it is the `class` attribute.
svgElement :: forall c a. Content c a => String -> Array (Attr a) -> c -> Node a
svgElement tag attrs = P.element tag (svgTraits_ attrs) <<< content

foreign import svgTraits_ :: forall a. Array (Attr a) -> Array (Attr a)

-- | What an element's children are: nodes, or keyed nodes. Which one is the
-- | type's business, so a list keyed in a function a hole calls is keyed
-- | too. Portico keys a list through its children, so the keys are already
-- | on the nodes (see `keyed`) and a keyed parent is an ordinary element.
class Content c a | c -> a where
  content :: c -> Markup a

instance Content (Array (P.Html a)) a where
  content = identity

instance Content (Array (Tuple String (P.Html a))) a where
  content = map snd

-- | An element function that takes either kind of children.
elementOf :: forall c a. Content c a => (Array (Attr a) -> Markup a -> Node a) -> Array (Attr a) -> c -> Node a
elementOf build attrs = build attrs <<< content

-- | No children.
none :: forall a. Markup a
none = []

-- | The node with the key added to its traits, as `P.key` would have put it
-- | there.
keyed :: forall k a. ToKey k => k -> Node a -> Tuple Key (Node a)
keyed k node = Tuple (toKey k) (runFn2 withKey_ (P.key (toKey k)) node)

foreign import withKey_ :: forall a. Fn2 (Attr a) (Node a) (Node a)

attribute :: forall a. String -> String -> Attr a
attribute = P.attribute

-- | What a child hole may hold, and the children it makes: nodes from a
-- | string, a node or markup, keyed nodes from keyed literals, and arrays
-- | and `Maybe`s of those. The dependency lets a hole's nodes fix the
-- | message type, as an annotation would, and whether the parent is keyed.
class Children c r | c -> r where
  children :: c -> r

instance Children String (Array (P.Html a)) where
  children s = [ P.text s ]

instance Children (P.Html a) (Array (P.Html a)) where
  children n = [ n ]

instance Children (Tuple String (P.Html a)) (Array (Tuple String (P.Html a))) where
  children kid = [ kid ]

instance Children c (Array x) => Children (Array c) (Array x) where
  children = Array.concatMap children

instance Children c (Array x) => Children (Maybe c) (Array x) where
  children = maybe [] children

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
