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
  , keyedHtmlElement
  , keyedSvgElement
  , svgNamespace
  , class Content
  , elementOf
  , none
  , keyed
  , attribute
  , class Children
  , children
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

element :: forall r c w a. Content c w a => String -> Array (Attr r a) -> c -> Node w a
element tag = elementOf (HH.element (HH.ElemName tag)) (keyedHtmlElement tag)

-- | Halogen's own keyed element, which halogen-vdom diffs by key.
keyedHtmlElement :: forall r w a. String -> Array (Attr r a) -> Array (Tuple Key (Node w a)) -> Node w a
keyedHtmlElement tag = HH.keyed (HH.ElemName tag)

-- | A keyed element in SVG's namespace.
keyedSvgElement :: forall r w a. String -> Array (Attr r a) -> Array (Tuple Key (Node w a)) -> Node w a
keyedSvgElement tag = HH.keyedNS svgNamespace (HH.ElemName tag)

svgNamespace :: HH.Namespace
svgNamespace = HH.Namespace "http://www.w3.org/2000/svg"

-- | What an element's children are: nodes, or keyed nodes. Which one is the
-- | type's business, so a list keyed in a function a hole calls is keyed
-- | too. Halogen keys at the parent, so an element function is given both
-- | ways to build its element and this picks one.
class Content c w a | c -> w a where
  elementOf
    :: forall r
     . (Array (Attr r a) -> Markup w a -> Node w a)
    -> (Array (Attr r a) -> Array (Tuple Key (Node w a)) -> Node w a)
    -> Array (Attr r a)
    -> c
    -> Node w a

instance Content (Array (HH.HTML w a)) w a where
  elementOf build _ = build

instance Content (Array (Tuple String (HH.HTML w a))) w a where
  elementOf _ build = build

-- | No children.
none :: forall w a. Markup w a
none = []

-- | Keys a child for its parent's keyed child list. A key belongs to the
-- | list, not to the element, so it is never an `Attr`: write `key` on the
-- | element in markup, never a key in a trait hole.
keyed :: forall k w a. ToKey k => k -> Node w a -> Tuple Key (Node w a)
keyed k node = Tuple (toKey k) node

attribute :: forall r a. String -> String -> Attr r a
attribute name = HH.attr (HH.AttrName name)

-- | What a child hole may hold, and the children it makes: nodes from a
-- | string, a node or markup, keyed nodes from keyed literals, and arrays
-- | and `Maybe`s of those. The dependency lets a hole's nodes fix the slot
-- | and action types, as an annotation would, and whether the parent is
-- | keyed.
class Children c r | c -> r where
  children :: c -> r

instance Children String (Array (HH.HTML w a)) where
  children s = [ HH.text s ]

instance Children (HH.HTML w a) (Array (HH.HTML w a)) where
  children n = [ n ]

instance Children (Tuple String (HH.HTML w a)) (Array (Tuple String (HH.HTML w a))) where
  children kid = [ kid ]

instance Children c (Array x) => Children (Array c) (Array x) where
  children = Array.concatMap children

instance Children c (Array x) => Children (Maybe c) (Array x) where
  children = maybe [] children

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
