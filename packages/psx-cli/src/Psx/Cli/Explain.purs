-- | `psx explain <element>`: what `psx-html-data` knows about an element,
-- | as a reader of markup needs it: the attributes it takes, each with its
-- | value type and the backend function lowered code calls, and its events.
module Psx.Cli.Explain (explain) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Psx.HtmlData (Element, Namespace(..), ValueType(..))
import Psx.HtmlData as HtmlData
import Psx.HtmlData.Table (table)

-- | The description of the element with this tag, in each namespace that has
-- | one (`a` is both an HTML and an SVG element).
explain :: String -> Either String String
explain tag = case Array.mapMaybe (\ns -> HtmlData.element ns tag) [ Html, Svg ] of
  [] -> Left ("no element <" <> tag <> "> in psx-html-data; a custom element's attributes are all strings")
  found -> Right (String.joinWith "\n\n" (map describe found))

describe :: Element -> String
describe e = String.joinWith "\n" $
  [ "<" <> e.name <> "> (" <> namespaceName e.namespace <> (if e.void then ", void" else "") <> "): B." <> e.purescript
  , ""
  , "Attributes:"
  ]
    <> rows (Array.mapMaybe attributeRow own)
    <> [ ""
       , "Global attributes (" <> show (Array.length globals) <> "): " <> String.joinWith ", " globals
       , ""
       , "Events:"
       ]
    <> rows (Array.mapMaybe eventRow (Array.sort e.events))
    <> [ ""
       , "Global events (" <> show (Array.length globalEvents) <> "): "
           <> String.joinWith ", " (Array.mapMaybe (map _.attribute <<< HtmlData.event) globalEvents)
       ]
  where
  entry = if e.namespace == Html then table.html else table.svg
  globals = entry.globalAttributes
  own = Array.sort (Array.filter (not <<< flip Array.elem globals) e.attributes)
  globalEvents = Array.filter (\name -> maybe false _.global (HtmlData.event name)) HtmlData.eventNames

  attributeRow name = HtmlData.elementAttribute e.namespace e.name name <#> \a ->
    [ name, "B." <> a.purescript, valueTypeName a.valueType ]

  eventRow name = HtmlData.event name <#> \ev ->
    [ ev.attribute, "B." <> ev.purescript, ev.interface ]

namespaceName :: Namespace -> String
namespaceName = case _ of
  Html -> "HTML"
  Svg -> "SVG"

valueTypeName :: ValueType -> String
valueTypeName = case _ of
  BooleanValue -> "boolean"
  StringValue -> "string"
  NumberValue -> "number"
  Enumeration values -> String.joinWith " | " (map show values)

-- | The first two columns padded to their widest cell.
rows :: Array (Array String) -> Array String
rows [] = [ "  (none)" ]
rows rs = map row rs
  where
  widths = map (\i -> Array.foldl max 0 (map (String.length <<< cell i) rs)) (Array.range 0 1)
  cell i r = case Array.index r i of
    Just c -> c
    Nothing -> ""
  row r = "  " <> String.joinWith "  " (Array.mapWithIndex (\i c -> maybe c (\w -> pad w c) (Array.index widths i)) r)
  pad w c = c <> String.joinWith "" (Array.replicate (w - String.length c) " ")
