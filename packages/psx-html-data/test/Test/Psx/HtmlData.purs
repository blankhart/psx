module Test.Psx.HtmlData where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Psx.HtmlData (Namespace(..), ValueType(..), attribute, attributeNames, element, elementAttribute, elementEvent, elementNames, event, eventByAttribute, eventNames, isHtmlElement, isSvgElement, isVoid)
import Psx.HtmlData.Names (camelCase, identifier)
import Test.Psx.HtmlData.Frameworks (Names, frameworks)

main :: Effect Unit
main = do
  voidElements
  svgNames
  nameMapping
  valueTypes
  events
  frameworkCoverage
  log "psx-html-data: all tests passed"

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual what expected actual =
  unless (expected == actual) $
    throw (what <> ": expected " <> show expected <> ", got " <> show actual)

assert :: String -> Boolean -> Effect Unit
assert what ok = unless ok $ throw what

voidElements :: Effect Unit
voidElements = do
  for_ [ "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr" ] \tag ->
    assert (tag <> " is void") (isVoid tag)
  for_ [ "div", "span", "p", "textarea", "script", "template", "svg", "path", "circle", "nope" ] \tag ->
    assert (tag <> " is not void") (not (isVoid tag))
  assertEqual "br's void flag" (Just true) (_.void <$> element Html "br")
  assertEqual "no SVG element is void" [] (Array.filter (\t -> map _.void (element Svg t) == Just true) (elementNames Svg))

svgNames :: Effect Unit
svgNames = do
  for_ [ "svg", "circle", "path", "g", "linearGradient", "feGaussianBlur", "foreignObject", "textPath", "clipPath" ] \tag ->
    assert (tag <> " is an SVG element") (isSvgElement tag)
  for_ [ "div", "button", "lineargradient", "foreignobject" ] \tag ->
    assert (tag <> " is not an SVG element") (not (isSvgElement tag))
  for_ [ "a", "script", "style", "title" ] \tag ->
    assert (tag <> " is HTML and SVG") (isHtmlElement tag && isSvgElement tag)
  assert "circle is not an HTML element" (not (isHtmlElement "circle"))
  -- Case matters: SVG attributes keep their camel case.
  assertEqual "svg takes viewBox" (Just "viewBox") (_.purescript <$> elementAttribute Svg "svg" "viewBox")
  assertEqual "svg does not take viewbox" Nothing (_.name <$> elementAttribute Svg "svg" "viewbox")
  assertEqual "button does not take viewBox" Nothing (_.name <$> elementAttribute Html "button" "viewBox")
  assertEqual "linearGradient's identifier" (Just "linearGradient") (_.purescript <$> element Svg "linearGradient")
  assertEqual "stroke-width on circle" (Just "strokeWidth") (_.purescript <$> elementAttribute Svg "circle" "stroke-width")

nameMapping :: Effect Unit
nameMapping = do
  -- The rules.
  assertEqual "camelCase accept-charset" "acceptCharset" (camelCase "accept-charset")
  assertEqual "camelCase xlink:href" "xlinkHref" (camelCase "xlink:href")
  assertEqual "camelCase viewBox" "viewBox" (camelCase "viewBox")
  assertEqual "identifier class" "class_" (identifier "class")
  assertEqual "identifier type" "type'" (identifier "type")
  assertEqual "identifier in" "in'" (identifier "in")
  assertEqual "identifier div" "div" (identifier "div")
  -- The table.
  let
    attr ns name = _.purescript <$> attribute ns name
    el ns name = _.purescript <$> element ns name
    ev name = _.purescript <$> eventByAttribute name
  assertEqual "class" (Just "class_") (attr Html "class")
  assertEqual "SVG class" (Just "class_") (attr Svg "class")
  assertEqual "type" (Just "type'") (attr Html "type")
  assertEqual "for" (Just "for") (attr Html "for")
  assertEqual "accept-charset" (Just "acceptCharset") (attr Html "accept-charset")
  assertEqual "http-equiv" (Just "httpEquiv") (attr Html "http-equiv")
  assertEqual "aria-label" (Just "ariaLabel") (attr Html "aria-label")
  assertEqual "SVG in" (Just "in'") (attr Svg "in")
  -- An attribute that shares an element's name takes `Attr`.
  assertEqual "title attribute" (Just "titleAttr") (attr Html "title")
  assertEqual "style attribute" (Just "styleAttr") (attr Html "style")
  assertEqual "SVG clip-path attribute" (Just "clipPathAttr") (attr Svg "clip-path")
  assertEqual "title element" (Just "title") (el Html "title")
  assertEqual "clipPath element" (Just "clipPath") (el Svg "clipPath")
  -- An element whose name is a keyword or a signature function.
  assertEqual "data element" (Just "data'") (el Html "data")
  assertEqual "SVG text element" (Just "textEl") (el Svg "text")
  assertEqual "font-face element" (Just "fontFace") (el Svg "font-face")
  -- Events.
  assertEqual "onclick" (Just "onClick") (ev "onclick")
  assertEqual "ondblclick" (Just "onDblClick") (ev "ondblclick")
  assertEqual "onmousedown" (Just "onMouseDown") (ev "onmousedown")
  assertEqual "onfocusin" (Just "onFocusIn") (ev "onfocusin")
  assertEqual "oncanplaythrough" (Just "onCanPlayThrough") (ev "oncanplaythrough")
  assertEqual "ongotpointercapture" (Just "onGotPointerCapture") (ev "ongotpointercapture")
  assertEqual "onloadedmetadata" (Just "onLoadedMetadata") (ev "onloadedmetadata")
  -- No two names share an identifier.
  let
    identifiers =
      Array.nub (Array.mapMaybe (el Html) (elementNames Html) <> Array.mapMaybe (el Svg) (elementNames Svg))
        <> Array.nub (Array.mapMaybe (attr Html) (attributeNames Html) <> Array.mapMaybe (attr Svg) (attributeNames Svg))
        <> Array.mapMaybe (\e -> _.purescript <$> event e) eventNames
  assertEqual "identifiers are unique" (Array.length (Array.nub identifiers)) (Array.length identifiers)

valueTypes :: Effect Unit
valueTypes = do
  let typeOf ns tag name = _.valueType <$> elementAttribute ns tag name
  assertEqual "disabled" (Just BooleanValue) (typeOf Html "button" "disabled")
  assertEqual "colspan" (Just IntegerValue) (typeOf Html "td" "colspan")
  assertEqual "href" (Just StringValue) (typeOf Html "a" "href")
  assertEqual "dir" (Just (Enumeration [ "ltr", "rtl", "auto" ])) (typeOf Html "div" "dir")
  assertEqual "button type" (Just (Enumeration [ "submit", "reset", "button" ])) (typeOf Html "button" "type")
  assertEqual "script type" (Just StringValue) (typeOf Html "script" "type")
  case typeOf Html "input" "type" of
    Just (Enumeration values) -> assert "input type includes checkbox" (Array.elem "checkbox" values)
    other -> throw ("input type: " <> show other)
  assertEqual "hidden" (Just BooleanValue) (typeOf Html "p" "hidden")
  assertEqual "value" (Just StringValue) (typeOf Html "input" "value")
  assertEqual "SVG width" (Just StringValue) (typeOf Svg "rect" "width")
  assertEqual "SVG pathLength" (Just NumberValue) (typeOf Svg "path" "pathLength")
  assertEqual "role is global" (Just StringValue) (typeOf Html "div" "role")
  assertEqual "div does not take href" Nothing (typeOf Html "div" "href")

events :: Effect Unit
events = do
  let iface name = _.interface <$> event name
  assertEqual "click" (Just "PointerEvent") (iface "click")
  assertEqual "keydown" (Just "KeyboardEvent") (iface "keydown")
  assertEqual "input" (Just "InputEvent") (iface "input")
  assertEqual "submit" (Just "SubmitEvent") (iface "submit")
  assertEqual "focus" (Just "FocusEvent") (iface "focus")
  assertEqual "change" (Just "Event") (iface "change")
  assertEqual "onclick on div" (Just "click") (_.name <$> elementEvent Html "div" "onclick")
  assertEqual "onclick on circle" (Just "click") (_.name <$> elementEvent Svg "circle" "onclick")
  assertEqual "onhashchange on body" (Just "hashchange") (_.name <$> elementEvent Html "body" "onhashchange")
  assertEqual "onhashchange on div" Nothing (_.name <$> elementEvent Html "div" "onhashchange")
  assertEqual "not an event" Nothing (_.name <$> eventByAttribute "onfoo")

-- | Names Portico or Screamer define that the table leaves out, each for a
-- | reason given on the issue that introduced the table.
knownDifferences :: Array String
knownDifferences =
  [ -- Not an element in any specification.
    "html element node"
  -- Elements from SVG 2 drafts, dropped before SVG 2 became a recommendation
  -- and never implemented. SVG Tiny 1.2's `solidColor` is in the table.
  , "svg element hatch"
  , "svg element hatchpath"
  , "svg element mesh"
  , "svg element meshgradient"
  , "svg element meshpatch"
  , "svg element meshrow"
  , "svg element solidcolor"
  -- Attributes removed from HTML, or never part of it.
  , "html attribute contextmenu"
  , "html attribute downloadAs"
  , "html attribute dropzone"
  , "html attribute pubdate"
  -- An event removed from HTML.
  , "event dragexit"
  ]

frameworkCoverage :: Effect Unit
frameworkCoverage = do
  let
    missing :: Names -> Array String
    missing names =
      map ("html element " <> _) (Array.filter (not <<< isHtmlElement) names.htmlElements)
        <> map ("svg element " <> _) (Array.filter (not <<< isSvgElement) names.svgElements)
        <> map ("html attribute " <> _) (Array.filter (isNothing <<< attribute Html) names.htmlAttributes)
        <> map ("svg attribute " <> _) (Array.filter (isNothing <<< attribute Svg) names.svgAttributes)
        <> map ("event " <> _) (Array.filter (isNothing <<< event) names.events)
    differences = Array.sort (Array.nub (missing frameworks.portico <> missing frameworks.screamer))
  assertEqual "names Portico and Screamer define that the table lacks" (Array.sort knownDifferences) differences
