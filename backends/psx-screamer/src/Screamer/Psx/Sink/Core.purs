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
  , class Content
  , content
  , none
  , keyed
  , attribute
  , class Children
  , children
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

element :: forall c a. Content c a => String -> Array (Attr a) -> c -> Node a
element tag attrs kids = Node \send -> S.element_ tag (map (attr send) attrs) (map (sink send) (content kids))

svgElement :: forall c a. Content c a => String -> Array (Attr a) -> c -> Node a
svgElement tag attrs kids = Node \send -> S.svgElement_ tag (map (attr send) attrs) (map (sink send) (content kids))

attr :: forall a. (a -> Effect Unit) -> Attr a -> S.Attribute
attr send (Attr a) = a send

-- | What an element's children are: nodes, or keyed nodes. A view runs
-- | once, so keyed children are created once and never reordered, and the
-- | keys are dropped; a list that changes is Screamer's `each` in a hole.
class Content c a | c -> a where
  content :: c -> Markup a

instance Content (Array (Node a)) a where
  content = identity

instance Content (Array (Tuple String (Node a))) a where
  content = map snd

-- | No children.
none :: forall a. Markup a
none = []

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

-- | What a child hole may hold, and the children it makes: nodes from a
-- | string, a node, markup, a Screamer element or a signal, keyed nodes
-- | from keyed literals, and arrays and `Maybe`s of those. The dependency
-- | lets a hole's nodes fix the message type, as an annotation would, and
-- | whether the parent is keyed.
class Children c r | c -> r where
  children :: c -> r

instance Children String (Array (Node a)) where
  children s = [ text s ]

instance Children (Node a) (Array (Node a)) where
  children n = [ n ]

instance Children (Tuple String (Node a)) (Array (Tuple String (Node a))) where
  children kid = [ kid ]

-- | A Screamer element, which sends nothing.
instance Children S.Element (Array (Node a)) where
  children e = [ Node \_ -> e ]

-- | A text node, updated in place.
instance Children (Signal String) (Array (Node a)) where
  children s = [ Node \_ -> S.text s ]

-- | A `dyn`: each new value replaces the nodes.
instance Children (Signal (Node a)) (Array (Node a)) where
  children s = [ Node \send -> S.dyn (sink send <$> s) ]

instance Children (Signal (Array (Node a))) (Array (Node a)) where
  children s = [ Node \send -> S.dyn (S.fragment_ <<< map (sink send) <$> s) ]

instance Children c (Array x) => Children (Array c) (Array x) where
  children = Array.concatMap children

instance Children c (Array x) => Children (Maybe c) (Array x) where
  children = maybe [] children

-- | The literal's only node, or a fragment of several.
root :: forall a. Markup a -> Node a
root nodes = case nodes of
  [ node ] -> node
  _ -> Node \send -> S.fragment_ (map (sink send) nodes)
