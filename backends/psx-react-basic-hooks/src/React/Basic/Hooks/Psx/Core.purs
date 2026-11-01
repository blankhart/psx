-- | JSX nodes and effect-valued handlers for PSX. Hooks stay in component setup.
module React.Basic.Hooks.Psx.Core
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
  , booleanAttribute
  , integerAttribute
  , numberAttribute
  , styleAttribute
  , handler
  , onValueInput
  , onKey
  ) where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (CSS)
import React.Basic as R

type Node :: Type -> Type
type Node a = JSX

foreign import data Attr :: Type -> Type

foreign import text :: forall a. String -> Node a

-- | What every markup literal denotes.
type Markup :: Type -> Type
type Markup a = Array JSX

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

element :: forall c a. Content c a => String -> Array (Attr a) -> c -> Node a
element tag attrs kids = createElement tag attrs (content kids)

-- | Plain children or keyed React subtrees.
class Content c a | c -> a where
  content :: c -> Markup a

instance Content (Array JSX) a where
  content = identity

instance Content (Array (Tuple String JSX)) a where
  content = map (\(Tuple key node) -> R.keyed key node)

-- | No children.
none :: forall a. Markup a
none = []

-- | Keys a child for its parent's keyed child list. A key belongs to the
-- | list, not to the element, so it is never an `Attr`: write `key` on the
-- | element in markup, never a key in a trait hole.
keyed :: forall k a. ToKey k => k -> Node a -> Tuple Key (Node a)
keyed k node = Tuple (toKey k) node

foreign import attribute :: forall a. String -> String -> Attr a
foreign import booleanAttribute :: forall a. String -> Boolean -> Attr a
foreign import integerAttribute :: forall a. String -> Int -> Attr a
foreign import numberAttribute :: forall a. String -> Number -> Attr a
foreign import styleAttribute :: forall a. CSS -> Attr a
foreign import handler :: String -> Effect Unit -> Attr (Effect Unit)
foreign import onValueInput :: (String -> Effect Unit) -> Attr (Effect Unit)
foreign import onKeyImpl :: String -> (String -> Effect Unit) -> Attr (Effect Unit)

-- | A key handler that runs the effect `f` gives for the event's key, if any.
onKey :: String -> (String -> Maybe (Effect Unit)) -> Attr (Effect Unit)
onKey name f = onKeyImpl name (fromMaybe (pure unit) <<< f)
foreign import createElement :: forall a. String -> Array (Attr a) -> Array JSX -> JSX

-- | What a child hole may hold, and the children it makes: nodes from a
-- | string, a node or markup, keyed nodes from keyed literals, and arrays
-- | and `Maybe`s of those. The dependency lets a hole's nodes fix the
-- | message type, as an annotation would, and whether the parent is keyed.
class Children c r | c -> r where
  children :: c -> r

instance Children String (Array JSX) where
  children s = [ text s ]

instance Children JSX (Array JSX) where
  children n = [ n ]

instance Children (Tuple String JSX) (Array (Tuple String JSX)) where
  children kid = [ kid ]

instance Children c (Array x) => Children (Array c) (Array x) where
  children = Array.concatMap children

instance Children c (Array x) => Children (Maybe c) (Array x) where
  children = maybe [] children

-- | The literal's only node, or a fragment of several, which renders as
-- | its nodes one after another.
root :: forall a. Markup a -> Node a
root nodes = case nodes of
  [ node ] -> node
  _ -> R.fragment nodes
