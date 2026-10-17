-- | `psx gen-backend`: a backend module's per-name functions and its
-- | conformance module, from `psx-html-data` and the framework's mapping.
-- |
-- | A mapping (`backends/mappings/<framework>.json`) says which of the
-- | framework's functions implements each element, attribute and event. Its
-- | templates cover a whole kind of name (every boolean attribute, every
-- | SVG element), its groups a list of names, and `names` one name each; the
-- | most specific wins. A template has a PureScript `type` and `code`, where
-- | `{name}` is the HTML name as a string literal, `{raw}` the bare name,
-- | `{ident}` the PureScript identifier and `{value}` the attribute's value
-- | type (`Boolean`, `String` or `Int`).
-- |
-- | The generated module re-exports the hand-written core (`core`, the names
-- | in `coreExports`) and adds
-- | one function per name. The conformance module uses every name as the
-- | lowering emits it, on an element that takes it (`B.button [ B.disabled
-- | true ] []`), so a backend that drifts from the signature, or a framework
-- | whose own types disagree with the table, fails to compile there.
module Psx.Cli.GenBackend
  ( Mapping
  , Template
  , Group
  , NameEntry
  , Generated
  , decodeMapping
  , generate
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Psx.HtmlData (Namespace(..), ValueType(..))
import Psx.HtmlData as HtmlData
import Psx.HtmlData.Table (table)

type Template = { type :: String, code :: String }

-- | A name's own entry; `sample` is the handler the conformance module
-- | passes an event, written with `x` for the message.
type NameEntry = { type :: Maybe String, code :: String, sample :: Maybe String }

type Group = { names :: Array String, type :: Maybe String, code :: String }

type Mapping =
  { module :: String
  -- | The source directory the modules are written under.
  , directory :: String
  , core :: String
  -- | What the generated module re-exports from the core.
  , coreExports :: Array String
  , doc :: Array String
  , imports :: Array String
  , element :: Template
  , voidElement :: Maybe Template
  , svgElement :: Maybe Template
  , boolean :: Template
  , string :: Template
  , number :: Template
  , event :: Template
  , groups :: Array Group
  , names :: Object NameEntry
  , conformance ::
      { imports :: Array String
      , core :: Array (Tuple String String)
      , extra :: Array String
      , nodes :: String
      , handlers :: String
      }
  }

type Generated = { backend :: String, conformance :: String }

-- Decoding -----------------------------------------------------------------------

decodeMapping :: Json -> Either String Mapping
decodeMapping json = do
  o <- object "the mapping" json
  let templates = Object.lookup "templates" o
  ts <- maybe (Left "no templates") (object "templates") templates
  conf <- field "conformance" o >>= object "conformance"
  confCore <- optionalArray "core" conf >>= traverse \entry -> do
    pair <- note "conformance.core holds [name, type] pairs" (Json.toArray entry)
    case pair of
      [ n, t ] -> Tuple <$> string "a core name" n <*> string "a core type" t
      _ -> Left "conformance.core holds [name, type] pairs"
  { module: _
  , directory: _
  , core: _
  , coreExports: _
  , doc: _
  , imports: _
  , element: _
  , voidElement: _
  , svgElement: _
  , boolean: _
  , string: _
  , number: _
  , event: _
  , groups: _
  , names: _
  , conformance: _
  }
    <$> (field "module" o >>= string "module")
    <*> (field "directory" o >>= string "directory")
    <*> (field "core" o >>= string "core")
    <*> (optionalArray "coreExports" o >>= traverse (string "a core export"))
    <*> (optionalArray "doc" o >>= traverse (string "a doc line"))
    <*> (optionalArray "imports" o >>= traverse (string "an import"))
    <*> template "element" ts
    <*> optionalTemplate "voidElement" ts
    <*> optionalTemplate "svgElement" ts
    <*> template "boolean" ts
    <*> template "string" ts
    <*> template "number" ts
    <*> template "event" ts
    <*> (optionalArray "groups" o >>= traverse group)
    <*> (maybe (pure Object.empty) (\n -> object "names" n >>= traverse nameEntry) (Object.lookup "names" o))
    <*> ( { imports: _, core: confCore, extra: _, nodes: _, handlers: _ }
            <$> (optionalArray "imports" conf >>= traverse (string "an import"))
            <*> (optionalArray "extra" conf >>= traverse (string "a line"))
            <*> (field "nodes" conf >>= string "conformance.nodes")
            <*> (field "handlers" conf >>= string "conformance.handlers")
        )
  where
  object what j = note (what <> " is not an object") (Json.toObject j)
  string what j = note (what <> " is not a string") (Json.toString j)
  field name o = note ("no " <> name) (Object.lookup name o)
  optionalArray name o = case Object.lookup name o of
    Nothing -> pure []
    Just j -> note (name <> " is not an array") (Json.toArray j)
  optionalString name o = traverse (string name) (Object.lookup name o)
  template name ts = field name ts >>= templateOf name
  optionalTemplate name ts = traverse (templateOf name) (Object.lookup name ts)
  templateOf name j = do
    t <- object name j
    { type: _, code: _ } <$> (field "type" t >>= string (name <> ".type")) <*> (field "code" t >>= string (name <> ".code"))
  group j = do
    g <- object "a group" j
    { names: _, type: _, code: _ }
      <$> (optionalArray "names" g >>= traverse (string "a group's name"))
      <*> optionalString "type" g
      <*> (field "code" g >>= string "a group's code")
  nameEntry j = do
    n <- object "a name" j
    { type: _, code: _, sample: _ }
      <$> optionalString "type" n
      <*> (field "code" n >>= string "a name's code")
      <*> optionalString "sample" n

-- Names ----------------------------------------------------------------------------

data Kind = ElementKind | AttributeKind | EventKind

-- | One generated function: its identifier, what it is, and how the
-- | conformance module uses it.
type Name =
  { ident :: String
  , kind :: Kind
  -- | The HTML name: a tag, an attribute or a DOM event type.
  , html :: String
  , namespace :: Namespace
  , void :: Boolean
  , value :: String
  -- | The elements the conformance module uses it on, each with the literal
  -- | the lowering would give it.
  , uses :: Array { tag :: String, namespace :: Namespace, literal :: String }
  }

elements :: Array Name
elements = html <> svg
  where
  html = HtmlData.elementNames Html # Array.mapMaybe \tag -> HtmlData.element Html tag <#> \e ->
    { ident: e.purescript, kind: ElementKind, html: tag, namespace: Html, void: e.void, value: "", uses: [] }
  svg = HtmlData.elementNames Svg # Array.mapMaybe \tag -> case HtmlData.element Svg tag of
    Just e | not (HtmlData.isHtmlElement tag) ->
      Just { ident: e.purescript, kind: ElementKind, html: tag, namespace: Svg, void: false, value: "", uses: [] }
    _ -> Nothing

-- | Attributes, one per identifier: an identifier HTML and SVG share is
-- | one function, used once in each namespace.
attributes :: Either String (Array Name)
attributes = traverse merge (Array.groupAllBy (comparing _.ident) entries)
  where
  entries = do
    ns <- [ Html, Svg ]
    name <- HtmlData.attributeNames ns
    attr <- Array.fromFoldable (HtmlData.attribute ns name)
    pure { ident: attr.purescript, name, namespace: ns }

  merge group = do
    let first = Array.head (Array.fromFoldable group)
    case first of
      Nothing -> Left "an empty group"
      Just a -> do
        let uses = Array.concatMap use (Array.fromFoldable group)
        values <- pure (Array.nub (map _.value uses))
        case values of
          [ value ] -> pure
            { ident: a.ident
            , kind: AttributeKind
            , html: a.name
            , namespace: a.namespace
            , void: false
            , value
            , uses: map _.use uses
            }
          _ -> Left ("`" <> a.ident <> "` has several value types: " <> String.joinWith ", " values)

  use entry = do
    let
      ns = entry.namespace
      entryTable = if ns == Html then table.html else table.svg
      global = Array.elem entry.name entryTable.globalAttributes
      tags = HtmlData.elementNames ns # Array.filter \tag ->
        maybe false (\e -> Array.elem entry.name e.attributes) (HtmlData.element ns tag)
      on = if global then Just (if ns == Html then "div" else "g") else Array.head tags
    tag <- Array.fromFoldable on
    attr <- Array.fromFoldable (HtmlData.elementAttribute ns tag entry.name)
    let { value, literal } = valueOf attr.valueType
    pure { value, use: { tag, namespace: ns, literal } }

  valueOf = case _ of
    BooleanValue -> { value: "Boolean", literal: "true" }
    NumberValue -> { value: "Int", literal: "0" }
    StringValue -> { value: "String", literal: "\"\"" }
    Enumeration values -> { value: "String", literal: maybe "\"\"" show (Array.head values) }

events :: Array Name
events = HtmlData.eventNames # Array.mapMaybe \name -> HtmlData.event name <#> \ev ->
  { ident: ev.purescript
  , kind: EventKind
  , html: name
  , namespace: Html
  , void: false
  , value: ""
  , uses: [ { tag: if ev.global then "div" else "body", namespace: Html, literal: "" } ]
  }

-- Generating -------------------------------------------------------------------------

generate :: String -> Mapping -> Either String Generated
generate source m = do
  attrs <- attributes
  let
    all = elements <> attrs <> events
    duplicates = map (Array.head <<< Array.fromFoldable) $ Array.filter ((_ > 1) <<< Array.length <<< Array.fromFoldable)
      (Array.groupAllBy (comparing identity) (map _.ident all))
  case Array.catMaybes duplicates of
    [] -> pure unit
    ds -> Left ("several names share the identifiers " <> String.joinWith ", " ds)
  case Array.filter (\n -> not (Array.any (\x -> x.ident == n) all)) (Object.keys m.names <> Array.concatMap _.names m.groups) of
    [] -> pure unit
    unknown -> Left ("the mapping names what psx-html-data does not: " <> String.joinWith ", " unknown)
  defs <- traverse (definition m) all
  pure
    { backend: backendModule source m all defs
    , conformance: conformanceModule source m elements attrs events
    }

header :: String -> String
header source = "-- Generated by psx gen-backend from " <> source <> " and psx-html-data. Do not edit."

-- | The function's type and code.
definition :: Mapping -> Name -> Either String { name :: Name, type :: String, code :: String }
definition m name = do
  let
    kindTemplate = case name.kind of
      ElementKind
        | name.void, Just t <- m.voidElement -> t
        | name.namespace == Svg, Just t <- m.svgElement -> t
        | otherwise -> m.element
      AttributeKind -> case name.value of
        "Boolean" -> m.boolean
        "Int" -> m.number
        _ -> m.string
      EventKind -> m.event
    fromGroup = Array.find (Array.elem name.ident <<< _.names) m.groups
    fromName = Object.lookup name.ident m.names
    type_ = fromMaybe kindTemplate.type ((fromName >>= _.type) <|> (fromGroup >>= _.type))
    code = case fromName, fromGroup of
      Just n, _ -> n.code
      _, Just g -> g.code
      _, _ -> kindTemplate.code
  pure { name, type: fill name type_, code: fill name code }

fill :: Name -> String -> String
fill name =
  replace "{name}" (show name.html)
    >>> replace "{raw}" name.html
    >>> replace "{ident}" name.ident
    >>> replace "{value}" name.value
  where
  replace from to = String.replaceAll (String.Pattern from) (String.Replacement to)

backendModule :: String -> Mapping -> Array Name -> Array { name :: Name, type :: String, code :: String } -> String
backendModule source m names defs = lines
  ( [ header source ] <> m.doc <>
      [ "module " <> m.module
      , "  ( module Core"
      ]
      <> map (\n -> "  , " <> n.ident) names
      <> [ "  ) where", "" ]
      <> map ("import " <> _) m.imports
      <> [ "import " <> m.core <> " (" <> String.joinWith ", " m.coreExports <> ") as Core" ]
      <> section "Elements" ElementKind
      <> section "Attributes" AttributeKind
      <> section "Events" EventKind
  )
  where
  section title kind =
    [ "", "-- " <> title <> " " <> String.joinWith "" (Array.replicate (76 - String.length title) "-") ]
      <> Array.concatMap def (Array.filter (sameKind kind <<< _.name) defs)
  def d = [ "", d.name.ident <> " :: " <> d.type, d.name.ident <> " = " <> d.code ]

sameKind :: Kind -> Name -> Boolean
sameKind k n = case k, n.kind of
  ElementKind, ElementKind -> true
  AttributeKind, AttributeKind -> true
  EventKind, EventKind -> true
  _, _ -> false

conformanceModule :: String -> Mapping -> Array Name -> Array Name -> Array Name -> String
conformanceModule source m els attrs evs = lines
  ( [ header source
    , "-- | Uses every name of `" <> m.module <> "` as lowered code does, so a backend"
    , "-- | that drifts from the PSX signature, or disagrees with psx-html-data,"
    , "-- | fails to compile here rather than in a user's view."
    , "module " <> m.module <> ".Conformance where"
    , ""
    ]
      <> map ("import " <> _) m.conformance.imports
      <> [ "import " <> m.module <> " as B" ]
      <> (if Array.null m.conformance.core then [] else [ "", "-- The hand-written core" ])
      <> Array.concatMap (\(Tuple n t) -> [ "", n <> " :: " <> t, n <> " = B." <> n ]) m.conformance.core
      <> (if Array.null m.conformance.extra then [] else [ "" ] <> m.conformance.extra)
      <> [ "", "-- | Every element, empty.", "elements :: " <> m.conformance.nodes, "elements =" ]
      <> list (map (\e -> "B." <> e.ident <> " [] []") els)
      <> [ "", "-- | Every attribute, with a literal value, on an element that takes it."
         , "attributes :: " <> m.conformance.nodes
         , "attributes ="
         ]
      <> list (Array.concatMap (\a -> map (\u -> on u ("B." <> a.ident <> " " <> u.literal)) a.uses) attrs)
      <> [ "", "-- | Every event, handled by `x`.", "events :: " <> m.conformance.handlers, "events x =" ]
      <> list (Array.concatMap (\e -> map (\u -> on u ("B." <> e.ident <> " " <> sample e)) e.uses) evs)
  )
  where
  identOf tag namespace = fromMaybe tag do
    e <- HtmlData.element namespace tag
    pure e.purescript
  on u attr = "B." <> identOf u.tag u.namespace <> " [ " <> attr <> " ] []"
  sample e = case Object.lookup e.ident m.names >>= _.sample of
    Just s -> parenthesise s
    Nothing -> "x"
  parenthesise s = if String.contains (String.Pattern " ") s then "(" <> s <> ")" else s
  list items = case Array.uncons items of
    Nothing -> [ "  []" ]
    Just { head, tail } -> [ "  [ " <> head ] <> map ("  , " <> _) tail <> [ "  ]" ]

lines :: Array String -> String
lines ls = String.joinWith "\n" ls <> "\n"

