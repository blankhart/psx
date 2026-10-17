-- | The hand-written core of `Psx.Static`, the PSX backend that renders
-- | markup to an HTML string. `Psx.Static` re-exports it beside the
-- | per-name functions `psx gen-backend` writes from
-- | `backends/mappings/static.json`.
-- |
-- | Nothing runs in a static page, so handlers render nothing and keys are
-- | dropped; a view is `Markup a` for any `a`, and `render` gives its HTML.
module Psx.Static.Core
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
  , render
  , renderMarkup
  , voidElement
  , booleanAttribute
  , noAttribute
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe, maybe)
import Data.String as String
import Data.Tuple (Tuple(..), snd)

data Node :: Type -> Type
data Node a
  = Text String
  | Element { tag :: String, void :: Boolean, attrs :: Array (Attr a), children :: Array (Node a) }
  | Fragment (Array (Node a))

data Attr :: Type -> Type
data Attr a
  = Attribute String String
  | BooleanAttribute String
  | NoAttribute

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
text = Text

element :: forall a. String -> Array (Attr a) -> Markup a -> Node a
element tag attrs kids = Element { tag, void: false, attrs, children: kids }

-- | An element with no end tag; its children, which the parser never gives
-- | it, are dropped.
voidElement :: forall a. String -> Array (Attr a) -> Markup a -> Node a
voidElement tag attrs _ = Element { tag, void: true, attrs, children: [] }

-- | A static page has no list to reorder, so the keys are dropped.
keyedElement :: forall a. String -> Array (Attr a) -> Array (Tuple Key (Node a)) -> Node a
keyedElement tag attrs kids = element tag attrs (map snd kids)

keyed :: forall k a. ToKey k => k -> Node a -> Tuple Key (Node a)
keyed k node = Tuple (toKey k) node

attribute :: forall a. String -> String -> Attr a
attribute = Attribute

-- | Written bare when true, left out when false.
booleanAttribute :: forall a. String -> Boolean -> Attr a
booleanAttribute name present = if present then BooleanAttribute name else NoAttribute

-- | What a handler renders: nothing.
noAttribute :: forall a. Attr a
noAttribute = NoAttribute

-- | What a child hole may hold. The dependency lets a hole's nodes fix the
-- | message type, as an annotation would.
class Children c a | c -> a where
  children :: c -> Markup a

instance Children String a where
  children s = [ Text s ]

instance Children (Node a) a where
  children n = [ n ]

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

-- | The literal's only node, or a fragment of several, which renders as
-- | its nodes one after another.
root :: forall a. Markup a -> Node a
root nodes = case nodes of
  [ node ] -> node
  _ -> Fragment nodes

-- | The node's HTML. Text is escaped, except inside `script` and `style`,
-- | whose text the HTML parser reads raw.
render :: forall a. Node a -> String
render = go false
  where
  go raw = case _ of
    Text s -> if raw then s else escapeText s
    Fragment nodes -> String.joinWith "" (map (go raw) nodes)
    Element e ->
      "<" <> e.tag <> String.joinWith "" (map renderAttr e.attrs) <> ">"
        <>
          if e.void then ""
          else String.joinWith "" (map (go (e.tag == "script" || e.tag == "style")) e.children) <> "</" <> e.tag <> ">"

-- | The markup's HTML: its nodes one after another.
renderMarkup :: forall a. Markup a -> String
renderMarkup = String.joinWith "" <<< map render

renderAttr :: forall a. Attr a -> String
renderAttr = case _ of
  Attribute name value -> " " <> name <> "=\"" <> escapeAttribute value <> "\""
  BooleanAttribute name -> " " <> name
  NoAttribute -> ""

escapeText :: String -> String
escapeText = replace "&" "&amp;" >>> replace "<" "&lt;" >>> replace ">" "&gt;"

escapeAttribute :: String -> String
escapeAttribute = replace "&" "&amp;" >>> replace "\"" "&quot;"

replace :: String -> String -> String -> String
replace from to = String.replaceAll (String.Pattern from) (String.Replacement to)
