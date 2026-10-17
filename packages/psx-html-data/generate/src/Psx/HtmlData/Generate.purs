-- | Generates `data/html.json`, and the module `Psx.HtmlData.Table` that
-- | carries the same table, from the web-platform datasets pinned in the
-- | workspace's `package.json`:
-- |
-- | - `html-tag-names`, `svg-tag-names` and `html-void-elements`: which
-- |   elements exist, which are HTML and which SVG, and which are void.
-- | - `html-element-attributes` and `svg-element-attributes`: the attributes
-- |   each element takes.
-- | - `property-information`: which attributes are boolean or numeric, and the
-- |   ARIA attributes.
-- | - `@webref/idl` and `@webref/events`: the event handlers HTML defines,
-- |   the events that fire at elements, and each event's interface.
-- |
-- | HTML's enumerated attributes are not in any of these, so their keywords
-- | are listed below (`overrides`), from the HTML standard's attribute index.
-- |
-- | Run from the workspace root: `spago run -p psx-html-data-generate`.
module Psx.HtmlData.Generate (main) where

import Prelude

import Control.Alternative (guard)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, for_, notElem)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.String.Pattern (Pattern(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Psx.HtmlData.Names (camelCase, identifier, signatureNames)

type Info =
  { boolean :: Boolean
  , booleanish :: Boolean
  , number :: Boolean
  -- A comma- or space-separated list.
  , list :: Boolean
  }

foreign import htmlTagNames :: Array String
foreign import svgTagNames :: Array String
foreign import htmlVoidElements :: Array String
foreign import htmlElementAttributes :: Object (Array String)
foreign import svgElementAttributes :: Object (Array String)
foreign import htmlInfo :: String -> Info
foreign import svgInfo :: String -> Info
foreign import ariaAttributes :: Array String

dataFile :: String
dataFile = "packages/psx-html-data/data/html.json"

tableModule :: String
tableModule = "packages/psx-html-data/src/Psx/HtmlData/Table.js"

-- | The datasets, whose versions the table records.
sourcePackages :: Array String
sourcePackages =
  [ "@webref/events", "@webref/idl", "html-element-attributes", "html-tag-names"
  , "html-void-elements", "property-information", "svg-element-attributes"
  , "svg-tag-names"
  ]

-- Value types -----------------------------------------------------------------

type ValueType = { kind :: String, values :: Array String }

booleanType :: ValueType
booleanType = { kind: "boolean", values: [] }

stringType :: ValueType
stringType = { kind: "string", values: [] }

numberType :: ValueType
numberType = { kind: "number", values: [] }

enumeration :: Array String -> ValueType
enumeration values = { kind: "enumeration", values }

-- | An attribute's type where the datasets give none or the wrong one.
-- | `elements` limits it to those elements; empty means every element.
type Override =
  { namespace :: String
  , attribute :: String
  , elements :: Array String
  , type :: ValueType
  }

html :: String -> Array String -> ValueType -> Override
html attribute elements t = { namespace: "html", attribute, elements, type: t }

svg :: String -> Array String -> ValueType -> Override
svg attribute elements t = { namespace: "svg", attribute, elements, type: t }

-- | HTML's enumerated attributes, from
-- | https://html.spec.whatwg.org/multipage/indices.html#attributes-3, and
-- | corrections to `property-information`, which describes attributes
-- | for hast rather than for authors.
overrides :: Array Override
overrides =
  [ html "autocapitalize" [] $ enumeration [ "off", "none", "on", "sentences", "words", "characters" ]
  , html "autocomplete" [ "form" ] $ enumeration [ "on", "off" ]
  , html "autocorrect" [] $ enumeration [ "on", "off" ]
  , html "contenteditable" [] $ enumeration [ "true", "false", "plaintext-only" ]
  , html "crossorigin" [] $ enumeration [ "anonymous", "use-credentials" ]
  , html "decoding" [] $ enumeration [ "sync", "async", "auto" ]
  , html "dir" [] $ enumeration [ "ltr", "rtl", "auto" ]
  -- An overloaded boolean that is almost always given a file name.
  , html "download" [] stringType
  , html "enctype" [] formEncodings
  , html "enterkeyhint" [] $ enumeration [ "enter", "done", "go", "next", "previous", "search", "send" ]
  , html "fetchpriority" [] $ enumeration [ "high", "low", "auto" ]
  , html "formenctype" [] formEncodings
  , html "formmethod" [] formMethods
  -- An overloaded boolean (`until-found` is its one other value) used as a boolean.
  , html "hidden" [] booleanType
  -- SVG's `width` and `height` are lengths, and one identifier serves both
  -- namespaces, so HTML's take a string too: a backend has one function each.
  , html "height" [] stringType
  , html "inputmode" [] $ enumeration [ "none", "text", "decimal", "numeric", "tel", "search", "email", "url" ]
  , html "kind" [ "track" ] $ enumeration [ "subtitles", "captions", "descriptions", "chapters", "metadata" ]
  , html "loading" [] $ enumeration [ "lazy", "eager" ]
  , html "method" [] formMethods
  , html "popover" [] $ enumeration [ "auto", "manual", "hint" ]
  , html "popovertargetaction" [] $ enumeration [ "toggle", "show", "hide" ]
  , html "preload" [] $ enumeration [ "none", "metadata", "auto" ]
  , html "referrerpolicy" [] $ enumeration
      [ "no-referrer", "no-referrer-when-downgrade", "same-origin", "origin", "strict-origin"
      , "origin-when-cross-origin", "strict-origin-when-cross-origin", "unsafe-url"
      ]
  , html "scope" [ "th" ] $ enumeration [ "row", "col", "rowgroup", "colgroup" ]
  -- Obsolete, and booleanish in `property-information`.
  , html "scrolling" [] $ enumeration [ "yes", "no", "auto" ]
  , html "shape" [ "area" ] $ enumeration [ "circle", "default", "poly", "rect" ]
  , html "translate" [] $ enumeration [ "yes", "no" ]
  , html "type" [ "button" ] $ enumeration [ "submit", "reset", "button" ]
  , html "type" [ "input" ] $ enumeration
      [ "hidden", "text", "search", "tel", "url", "email", "password", "date", "month", "week"
      , "time", "datetime-local", "number", "range", "color", "checkbox", "radio", "file"
      , "submit", "image", "reset", "button"
      ]
  , html "type" [ "ol" ] $ enumeration [ "1", "a", "A", "i", "I" ]
  -- `property-information` marks `value` booleanish for hast's sake.
  , html "value" [] stringType
  , html "wrap" [] $ enumeration [ "soft", "hard" ]
  , html "width" [] stringType
  , html "writingsuggestions" [] $ enumeration [ "true", "false" ]
  , html "aria-checked" [] tristate
  , html "aria-pressed" [] tristate
  , svg "aria-checked" [] tristate
  , svg "aria-pressed" [] tristate
  , svg "download" [] stringType
  , svg "externalResourcesRequired" [] $ enumeration [ "false", "true" ]
  , svg "preserveAlpha" [] $ enumeration [ "false", "true" ]
  ]
  where
  formEncodings = enumeration [ "application/x-www-form-urlencoded", "multipart/form-data", "text/plain" ]
  formMethods = enumeration [ "get", "post", "dialog" ]
  tristate = enumeration [ "false", "mixed", "true" ]

-- | The type the datasets give an attribute.
datasetType :: Info -> ValueType
datasetType info
  | info.boolean = booleanType
  | info.number && not info.list = numberType
  | info.booleanish = enumeration [ "false", "true" ]
  | otherwise = stringType

-- Namespaces ------------------------------------------------------------------

type Namespace =
  { name :: String
  , tags :: Array String
  , isVoid :: String -> Boolean
  , elementAttributes :: Object (Array String)
  , info :: String -> Info
  }

namespaces :: Array Namespace
namespaces =
  [ { name: "html"
    , tags: htmlTagNames
    , isVoid: \tag -> Array.elem tag htmlVoidElements
    , elementAttributes: htmlElementAttributes
    , info: htmlInfo
    }
  , { name: "svg"
    , tags: svgTagNames
    , isVoid: const false
    , elementAttributes: svgElementAttributes
    , info: svgInfo
    }
  ]

globalAttributes :: Namespace -> Array String
globalAttributes ns =
  Array.sort $ Array.nub $ fromMaybe [] (Object.lookup "*" ns.elementAttributes) <> ariaAttributes

ownAttributes :: Namespace -> String -> Array String
ownAttributes ns tag = Array.sort $ Array.nub $ fromMaybe [] $ Object.lookup tag ns.elementAttributes

allAttributes :: Namespace -> Array String
allAttributes ns =
  Array.sort $ Array.nub $ globalAttributes ns <> Array.concatMap (ownAttributes ns) ns.tags

attributeType :: Namespace -> String -> ValueType
attributeType ns attribute =
  case Array.find (\o -> o.namespace == ns.name && o.attribute == attribute && Array.null o.elements) overrides of
    Just o -> o.type
    Nothing -> datasetType (ns.info attribute)

elementTypes :: Namespace -> String -> Array (Tuple String ValueType)
elementTypes ns attribute = Array.sortWith (\(Tuple e _) -> e) do
  o <- Array.filter (\o -> o.namespace == ns.name && o.attribute == attribute) overrides
  element <- o.elements
  pure (Tuple element o.type)

-- Events ----------------------------------------------------------------------

type WebrefEvent = { type :: String, interface :: Maybe String, targets :: Array String }

-- | Events left out: legacy aliases and incubating proposals.
excludedEvents :: Array String
excludedEvents =
  [ "DOMActivate", "DOMFocusIn", "DOMFocusOut", "textInput"
  , "beforexrselect", "fencedtreeclick", "installresult", "navbeforefocus", "navnotarget"
  ]

isExcluded :: String -> Boolean
isExcluded event =
  Array.elem event excludedEvents || String.take 6 event == "webkit"

-- | The base interfaces of all HTML and SVG elements.
isBaseTarget :: String -> Boolean
isBaseTarget t = Array.elem t [ "Element", "HTMLElement", "SVGElement" ]

isElementTarget :: String -> Boolean
isElementTarget t =
  isBaseTarget t
    || (startsWith "HTML" t || startsWith "SVG" t) && endsWith "Element" t

decodeEvents :: Json -> Maybe (Array WebrefEvent)
decodeEvents json = Json.toArray json >>= traverse \entry -> do
  o <- Json.toObject entry
  t <- Object.lookup "type" o >>= Json.toString
  let iface = Object.lookup "interface" o >>= Json.toString
  targets <- case Object.lookup "targets" o of
    Nothing -> pure []
    Just ts -> Json.toArray ts >>= traverse (\x -> Json.toObject x >>= Object.lookup "target" >>= Json.toString)
  pure { type: t, interface: iface, targets }

-- | The event handlers an IDL mixin in `html.idl` declares, without `on`.
mixinHandlers :: String -> String -> Effect (Array String)
mixinHandlers idl mixin =
  case String.indexOf (Pattern ("interface mixin " <> mixin <> " {")) idl of
    Nothing -> throw ("No mixin " <> mixin <> " in html.idl")
    Just start -> do
      let
        rest = String.drop start idl
        block = maybe rest (\end -> String.take end rest) (String.indexOf (Pattern "};") rest)
        handler line =
          case Array.last (String.split (Pattern " ") (String.trim line)) of
            Just word
              | startsWith "attribute " (String.trim line)
              , String.contains (Pattern "EventHandler on") line ->
                  Just (String.drop 2 (String.takeWhile (_ /= String.codePointFromChar ';') word))
            _ -> Nothing
      pure (Array.mapMaybe handler (String.split (Pattern "\n") block))

eventInterface :: Array WebrefEvent -> String -> Effect String
eventInterface events name = do
  let
    candidates = Array.filter (\e -> e.type == name && e.interface /= Just "DOMException") events
    onElements = Array.filter (any isElementTarget <<< _.targets) candidates
    preferred = if Array.null onElements then candidates else onElements
    interfaces = Array.nub (Array.mapMaybe _.interface preferred)
  case Array.filter (_ /= "Event") interfaces of
    [] | not (Array.null interfaces) -> pure "Event"
    [ one ] -> pure one
    [] -> throw ("No interface for the event " <> name <> " in @webref/events")
    several -> throw ("Several interfaces for the event " <> name <> ": " <> show several)

-- | Words for splitting event names, which are written run together.
eventWords :: Array String
eventWords =
  [ "abort", "after", "animation", "auto", "aux", "before", "blur", "can", "cancel"
  , "capture", "change", "click", "close", "command", "composition", "content", "context"
  , "copy", "cue", "cut", "data", "dbl", "down", "drag", "drop", "duration", "emptied", "end"
  , "ended", "enter", "error", "focus", "form", "fullscreen", "got", "handled", "hash", "hide"
  , "in", "input", "invalid", "iteration", "key", "language", "leave", "load", "loaded"
  , "lost", "match", "menu", "message", "metadata", "mouse", "move", "offline", "online", "out"
  , "over", "page", "paste", "pause", "play", "playing", "pointer", "policy", "pop", "press"
  , "print", "progress", "rate", "raw", "rejection", "reset", "resize", "restored", "reveal"
  , "run", "scroll", "security", "seeked", "seeking", "select", "selection", "show", "slot"
  , "stalled", "start", "state", "storage", "submit", "suspend", "swap", "through", "time"
  , "toggle", "touch", "transition", "unhandled", "unload", "up", "update", "violation"
  , "visibility", "volume", "waiting", "wheel"
  ]

-- | Splits a run-together name into the fewest words from `eventWords`.
segment :: String -> Maybe (Array String)
segment s
  | s == "" = Just []
  | otherwise = Array.head $ Array.sortWith Array.length do
      word <- eventWords
      guard (startsWith word s)
      rest <- Array.fromFoldable (segment (String.drop (String.length word) s))
      pure (Array.cons word rest)

eventIdentifier :: String -> Effect String
eventIdentifier name = case segment name of
  Just words -> pure ("on" <> String.joinWith "" (map capitalise words))
  Nothing -> throw ("Cannot split the event name " <> name <> "; add its words to eventWords")

-- Identifiers -----------------------------------------------------------------

-- | Element identifiers: `identifier`, or the camel-cased name with `El` when
-- | that is one of the signature's own names (SVG's `text`).
elementIdentifier :: String -> String
elementIdentifier tag =
  let id = identifier tag in if Array.elem id signatureNames then camelCase tag <> "El" else id

-- | Attribute identifiers: `identifier`, or the camel-cased name with `Attr`
-- | when that names an element or a signature function (`title`, `style`).
attributeIdentifier :: Set.Set String -> String -> String
attributeIdentifier taken attribute =
  let id = identifier attribute in if Set.member id taken then camelCase attribute <> "Attr" else id

-- | Fails if two different names were given one identifier.
checkUnique :: String -> Array (Tuple String String) -> Effect Unit
checkUnique what pairs = do
  let
    byId = Map.fromFoldableWith Set.union (map (\(Tuple name id) -> Tuple id (Set.singleton name)) pairs)
  for_ (Map.toUnfoldable byId :: Array _) \(Tuple id names) ->
    when (Set.size names > 1) $ throw
      ("The " <> what <> " " <> show (Array.fromFoldable names) <> " all map to " <> id)

-- Output ----------------------------------------------------------------------

object :: Array (Tuple String Json) -> Json
object = Json.fromObject <<< Object.fromFoldable

strings :: Array String -> Json
strings = Json.fromArray <<< map Json.fromString

valueTypeJson :: ValueType -> Json
valueTypeJson t = object
  [ Tuple "kind" (Json.fromString t.kind)
  , Tuple "values" (strings t.values)
  ]

main :: Effect Unit
main = do
  unlessM (FS.exists "node_modules/@webref/events/events.json") $
    throw "Run from the workspace root after npm ci: node_modules/@webref/events is missing"

  versions <- for sourcePackages \p -> do
    text <- FS.readTextFile UTF8 ("node_modules/" <> p <> "/package.json")
    case jsonParser text of
      Right json | Just v <- Json.toObject json >>= Object.lookup "version" >>= Json.toString ->
        pure (Tuple p (Json.fromString v))
      _ -> throw ("No version in " <> p <> "/package.json")

  eventsText <- FS.readTextFile UTF8 "node_modules/@webref/events/events.json"
  webrefEvents <- case jsonParser eventsText >>= (maybe (Left "unexpected shape") Right <<< decodeEvents) of
    Right es -> pure es
    Left err -> throw ("Cannot read @webref/events: " <> err)
  idl <- FS.readTextFile UTF8 "node_modules/@webref/idl/html.idl"
  globalHandlers <- mixinHandlers idl "GlobalEventHandlers"
  windowHandlers <- mixinHandlers idl "WindowEventHandlers"

  let
    elementEvents = map _.type $ Array.filter (any isBaseTarget <<< _.targets) webrefEvents
    eventNames = Array.sort $ Array.nub $ Array.filter (not <<< isExcluded) $
      globalHandlers <> windowHandlers <> elementEvents
    -- WindowEventHandlers are content attributes of `body` and `frameset` only.
    windowOnly = Array.filter (\e -> Array.elem e windowHandlers && notElem e globalHandlers) eventNames
    windowElements = [ "body", "frameset" ]

  events <- for eventNames \name -> do
    iface <- eventInterface webrefEvents name
    id <- eventIdentifier name
    pure { name, iface, id, global: notElem name windowOnly }

  let
    tagPairs = Array.concatMap (\ns -> map (\t -> Tuple t (elementIdentifier t)) ns.tags) namespaces
    elementIds = Set.fromFoldable (map (\(Tuple _ id) -> id) tagPairs <> signatureNames)
    attributePairs = Array.nub $ Array.concatMap
      (\ns -> map (\a -> Tuple a (attributeIdentifier elementIds a)) (allAttributes ns))
      namespaces
    eventPairs = map (\e -> Tuple ("on" <> e.name) e.id) events
  checkUnique "elements" (Array.nub tagPairs)
  checkUnique "attributes" attributePairs
  checkUnique "names" (Array.nub tagPairs <> attributePairs <> eventPairs)

  -- Every element-specific override must name an attribute the element takes.
  for_ overrides \o -> for_ o.elements \element ->
    case Array.find (\ns -> ns.name == o.namespace) namespaces of
      Just ns | Array.elem o.attribute (ownAttributes ns element <> globalAttributes ns) -> pure unit
      _ -> throw ("The override for " <> o.attribute <> " names " <> element <> ", which does not take it")

  let
    namespaceJson ns = object
      [ Tuple "elements" $ object $ Array.sort ns.tags <#> \tag -> Tuple tag $ object
          [ Tuple "purescript" (Json.fromString (elementIdentifier tag))
          , Tuple "void" (Json.fromBoolean (ns.isVoid tag))
          , Tuple "attributes" (strings (ownAttributes ns tag))
          , Tuple "events" (strings (if ns.name == "html" && Array.elem tag windowElements then windowOnly else []))
          ]
      , Tuple "globalAttributes" (strings (globalAttributes ns))
      , Tuple "attributes" $ object $ allAttributes ns <#> \attribute -> Tuple attribute $ object
          [ Tuple "purescript" (Json.fromString (attributeIdentifier elementIds attribute))
          , Tuple "type" (valueTypeJson (attributeType ns attribute))
          , Tuple "elementTypes" $ object $ map valueTypeJson <$> elementTypes ns attribute
          ]
      ]
    table = object
      [ Tuple "sources" (object versions)
      , Tuple "html" (maybe Json.jsonNull namespaceJson (Array.index namespaces 0))
      , Tuple "svg" (maybe Json.jsonNull namespaceJson (Array.index namespaces 1))
      , Tuple "events" $ object $ events <#> \e -> Tuple e.name $ object
          [ Tuple "attribute" (Json.fromString ("on" <> e.name))
          , Tuple "purescript" (Json.fromString e.id)
          , Tuple "interface" (Json.fromString e.iface)
          , Tuple "global" (Json.fromBoolean e.global)
          ]
      ]
    text = Json.stringifyWithIndent 2 table

  FS.writeTextFile UTF8 dataFile (text <> "\n")
  FS.writeTextFile UTF8 tableModule $
    "// Generated by psx-html-data-generate. Do not edit: run `spago run -p psx-html-data-generate`.\n"
      <> "// The same table as data/html.json.\n"
      <> "export const table = "
      <> text
      <> ";\n"
  log ("Wrote " <> dataFile <> " and " <> tableModule)

startsWith :: String -> String -> Boolean
startsWith prefix s = String.take (String.length prefix) s == prefix

endsWith :: String -> String -> Boolean
endsWith suffix s = String.drop (String.length s - String.length suffix) s == suffix

capitalise :: String -> String
capitalise s = case CodeUnits.uncons s of
  Nothing -> s
  Just { head, tail } -> String.toUpper (CodeUnits.singleton head) <> tail
