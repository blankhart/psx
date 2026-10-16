-- | HTML elements, attributes and events, as data.
-- |
-- | The table is generated into `data/html.json` (and `Psx.HtmlData.Table`)
-- | by `spago run -p psx-html-data-generate`. Names are case-sensitive, as
-- | SVG's are: HTML names are lower case.
module Psx.HtmlData
  ( Attribute
  , Element
  , Event
  , Namespace(..)
  , ValueType(..)
  , attribute
  , attributeNames
  , element
  , elementAttribute
  , elementEvent
  , elementNames
  , event
  , eventByAttribute
  , eventNames
  , isHtmlElement
  , isSvgElement
  , isVoid
  , sources
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Psx.HtmlData.Table (NamespaceEntry, ValueTypeEntry, table)

data Namespace = Html | Svg

derive instance Eq Namespace
derive instance Ord Namespace

instance Show Namespace where
  show Html = "Html"
  show Svg = "Svg"

data ValueType
  = BooleanValue
  | StringValue
  | NumberValue
  -- | An enumerated attribute, with its keywords.
  | Enumeration (Array String)

derive instance Eq ValueType

instance Show ValueType where
  show BooleanValue = "BooleanValue"
  show StringValue = "StringValue"
  show NumberValue = "NumberValue"
  show (Enumeration values) = "(Enumeration " <> show values <> ")"

-- | An element. `attributes` are its own, not its namespace's global ones,
-- | and `events` are those it takes beside the global ones (`body`'s window
-- | events).
type Element =
  { namespace :: Namespace
  , name :: String
  , purescript :: String
  , void :: Boolean
  , attributes :: Array String
  , events :: Array String
  }

-- | An attribute, with its type on most elements.
type Attribute =
  { namespace :: Namespace
  , name :: String
  , purescript :: String
  , valueType :: ValueType
  }

-- | An event, named by its DOM type (`click`). `attribute` is the name markup
-- | uses (`onclick`) and `interface` the DOM interface of the event object a
-- | handler receives (`PointerEvent`). A `global` event applies to every
-- | element; any other only to elements that list it.
type Event =
  { name :: String
  , attribute :: String
  , purescript :: String
  , interface :: String
  , global :: Boolean
  }

namespaceEntry :: Namespace -> NamespaceEntry
namespaceEntry Html = table.html
namespaceEntry Svg = table.svg

valueType :: ValueTypeEntry -> ValueType
valueType t = case t.kind of
  "boolean" -> BooleanValue
  "number" -> NumberValue
  "enumeration" -> Enumeration t.values
  _ -> StringValue

-- | The element with this tag name in the namespace.
element :: Namespace -> String -> Maybe Element
element ns name = Object.lookup name (namespaceEntry ns).elements <#> \e ->
  { namespace: ns
  , name
  , purescript: e.purescript
  , void: e.void
  , attributes: e.attributes
  , events: e.events
  }

-- | Whether the tag names an HTML element.
isHtmlElement :: String -> Boolean
isHtmlElement = isJust <<< element Html

-- | Whether the tag names an SVG element. `a`, `script`, `style` and `title`
-- | are both.
isSvgElement :: String -> Boolean
isSvgElement = isJust <<< element Svg

-- | Whether the tag names a void HTML element, which has no end tag or
-- | children. No SVG element is void.
isVoid :: String -> Boolean
isVoid = maybe false _.void <<< element Html

-- | The attribute with this name in the namespace, wherever it applies.
attribute :: Namespace -> String -> Maybe Attribute
attribute ns name = Object.lookup name (namespaceEntry ns).attributes <#> \a ->
  { namespace: ns, name, purescript: a.purescript, valueType: valueType a.type }

-- | The attribute as the element takes it, with the element's type for it,
-- | or `Nothing` if the element does not take it. `data-*` attributes and
-- | event attributes are not in the table; see `elementEvent`.
elementAttribute :: Namespace -> String -> String -> Maybe Attribute
elementAttribute ns tag name = do
  let entry = namespaceEntry ns
  e <- Object.lookup tag entry.elements
  a <- Object.lookup name entry.attributes
  if Array.elem name e.attributes || Array.elem name entry.globalAttributes then
    Just
      { namespace: ns
      , name
      , purescript: a.purescript
      , valueType: valueType (maybe a.type identity (Object.lookup tag a.elementTypes))
      }
  else Nothing

-- | The event with this DOM type (`click`).
event :: String -> Maybe Event
event name = Object.lookup name table.events <#> \e ->
  { name
  , attribute: e.attribute
  , purescript: e.purescript
  , interface: e.interface
  , global: e.global
  }

-- | The event whose handler attribute this is (`onclick`).
eventByAttribute :: String -> Maybe Event
eventByAttribute name = String.stripPrefix (String.Pattern "on") name >>= event

-- | The event a handler attribute (`onclick`) names, if the element takes it.
elementEvent :: Namespace -> String -> String -> Maybe Event
elementEvent ns tag name = do
  e <- Object.lookup tag (namespaceEntry ns).elements
  ev <- eventByAttribute name
  if ev.global || Array.elem ev.name e.events then Just ev else Nothing

-- | Every element's tag name in the namespace, sorted.
elementNames :: Namespace -> Array String
elementNames = sortedKeys <<< _.elements <<< namespaceEntry

-- | Every attribute name in the namespace, sorted.
attributeNames :: Namespace -> Array String
attributeNames = sortedKeys <<< _.attributes <<< namespaceEntry

-- | Every event's DOM type, sorted.
eventNames :: Array String
eventNames = sortedKeys table.events

-- | The npm packages the table was generated from, with their versions.
sources :: Object String
sources = table.sources

sortedKeys :: forall a. Object a -> Array String
sortedKeys = Array.sort <<< Object.keys
