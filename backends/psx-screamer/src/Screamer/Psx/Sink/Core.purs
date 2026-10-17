-- | The hand-written core of `Screamer.Psx.Sink`, the PSX backend that hosts
-- | a message-passing view inside Screamer. `Screamer.Psx.Sink` re-exports
-- | it beside the per-name functions `psx gen-backend` writes from
-- | `backends/mappings/screamer-sink.json`.
-- |
-- | A `Node Msg` is a Screamer element waiting for the sink its handlers
-- | send messages to, so a view is written as on Portico or Halogen, with
-- | `onclick={Increment}`, and `sink` turns it into a Screamer `Element`.
-- | Each node and attribute is one closure over the sink. Holes take what
-- | `Screamer.Psx` holes take, signals included, and a Screamer `Element`.
module Screamer.Psx.Sink.Core
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
  , keyed
  , attribute
  , class Children
  , children
  , class KeyedChildren
  , keyedChildren
  , root
  , sink
  , svgElement
  , constant
  , handler
  , valueHandler
  , keyHandler
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Screamer.Core as S
import Screamer.Signal (Signal)
import Web.Event.Event (Event)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

newtype Node a = Node ((a -> Effect Unit) -> S.Element)

newtype Attr a = Attr ((a -> Effect Unit) -> S.Attribute)

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

-- | The Screamer element a view denotes, its messages sent to the sink.
sink :: forall a. (a -> Effect Unit) -> Node a -> S.Element
sink send (Node node) = node send

text :: forall a. String -> Node a
text s = Node \_ -> S.text (pure s)

element :: forall a. String -> Array (Attr a) -> Markup a -> Node a
element tag attrs kids = Node \send -> S.element_ tag (map (attr send) attrs) (map (sink send) kids)

svgElement :: forall a. String -> Array (Attr a) -> Markup a -> Node a
svgElement tag attrs kids = Node \send -> S.svgElement_ tag (map (attr send) attrs) (map (sink send) kids)

attr :: forall a. (a -> Effect Unit) -> Attr a -> S.Attribute
attr send (Attr a) = a send

-- | A view runs once, so keyed children are created once and never
-- | reordered; a list that changes is Screamer's `each` in a hole.
-- | `Screamer.Psx.Sink.keyedElement` picks this or `keyedSvgElement` by
-- | the tag.
keyedHtmlElement :: forall a. String -> Array (Attr a) -> Array (Tuple Key (Node a)) -> Node a
keyedHtmlElement tag attrs kids = element tag attrs (map snd kids)

keyedSvgElement :: forall a. String -> Array (Attr a) -> Array (Tuple Key (Node a)) -> Node a
keyedSvgElement tag attrs kids = svgElement tag attrs (map snd kids)

keyed :: forall k a. ToKey k => k -> Node a -> Tuple Key (Node a)
keyed k node = Tuple (toKey k) node

attribute :: forall a. String -> String -> Attr a
attribute name value = constant (S.attribute name (pure value))

-- | A Screamer attribute that sends nothing.
constant :: forall a. S.Attribute -> Attr a
constant a = Attr \_ -> a

-- | A listener that sends the message.
handler :: forall a. String -> a -> Attr a
handler name msg = Attr \send -> S.on_ name \_ -> send msg

-- | A listener given the `value` of the element the event came from.
valueHandler :: forall a. String -> (String -> a) -> Attr a
valueHandler name f = Attr \send -> S.on_ name \event -> targetValue event >>= send <<< f

-- | A listener given the keyboard event.
keyHandler :: forall a. String -> (KeyboardEvent -> a) -> Attr a
keyHandler name f = Attr \send -> S.unsafeOn_ name (send <<< f)

foreign import targetValue :: Event -> Effect String

-- | What a child hole may hold. The dependency lets a hole's nodes fix the
-- | message type, as an annotation would.
class Children c a | c -> a where
  children :: c -> Markup a

instance Children String a where
  children s = [ text s ]

instance Children (Node a) a where
  children n = [ n ]

-- | A Screamer element, which sends nothing.
instance Children S.Element a where
  children e = [ Node \_ -> e ]

-- | A text node, updated in place.
instance Children (Signal String) a where
  children s = [ Node \_ -> S.text s ]

-- | A `dyn`: each new value replaces the nodes.
instance Children (Signal (Node a)) a where
  children s = [ Node \send -> S.dyn (sink send <$> s) ]

instance Children (Signal (Array (Node a))) a where
  children s = [ Node \send -> S.dyn (S.fragment_ <<< map (sink send) <$> s) ]

instance Children c a => Children (Array c) a where
  children = Array.concatMap children

instance Children c a => Children (Maybe c) a where
  children = maybe [] children

-- | What a child hole of a keyed parent may hold: keyed literals, or arrays
-- | of them.
class KeyedChildren c a | c -> a where
  keyedChildren :: c -> Array (Tuple Key (Node a))

instance KeyedChildren (Tuple String (Node a)) a where
  keyedChildren kid = [ kid ]

instance KeyedChildren c a => KeyedChildren (Array c) a where
  keyedChildren = Array.concatMap keyedChildren

instance KeyedChildren c a => KeyedChildren (Maybe c) a where
  keyedChildren = maybe [] keyedChildren

-- | The literal's only node, or a fragment of several.
root :: forall a. Markup a -> Node a
root nodes = case nodes of
  [ node ] -> node
  _ -> Node \send -> S.fragment_ (map (sink send) nodes)
