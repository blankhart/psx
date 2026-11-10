-- | Language operations stay in PureScript; the FFI owns Node streams/processes.
module Psx.LanguageServer where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe, isJust, fromMaybe)
import Data.String as String
import Data.String.CodeUnits as SCU
import Psx.Cst.Source (source, posOf, offsetOf)
import PureScript.CST.Types (SourcePos)
import Effect (Effect)
import Psx.Cst (PsxError, Markup(..), Child(..), parseModule, foldMapMarkupRoots)
import Psx.Cst.Psxt (parsePsxt)
import Psx.Format (formatPsxWith, formatPsxtWith)
import Psx.Format.Config (loadConfig)
import Psx.HtmlData as Html
import Psx.Lower (lower, lowerPsxt)
import Psx.Lower.PositionMap (PositionMap, SegmentKind(..), before)

type Position = { line :: Int, character :: Int }
type Mapped = { valid :: Boolean, position :: Position }
type Analysis =
  { valid :: Boolean
  , purs :: String
  , diagnostics :: Array PsxError
  , forward :: Position -> Mapped
  , backward :: Position -> Mapped
  }

unmapped :: Position -> Mapped
unmapped position = { valid: false, position }

-- | LSP uses UTF-16 columns; CST and the existing lowering maps count code
-- | points. Only copied spans are exact enough for compiler edits/navigation.
mapPoint :: String -> String -> PositionMap -> Boolean -> Position -> Mapped
mapPoint original generated pm inverse position = do
  let
    input = source (if inverse then generated else original)
    output = source (if inverse then original else generated)
    lineStart src line = fromMaybe (SCU.length src.text) (Array.index src.lineStarts line)
    point = posOf input (lineStart input position.line + position.character)
    ranges segment = if inverse then { from: segment.generated, to: segment.source } else { from: segment.source, to: segment.generated }
    exact segment = segment.kind == Copy && (let r = (ranges segment).from in not (before point r.start) && before point r.end)
    endpoint segment = segment.kind == Copy && point == (ranges segment).from.end
    found = case Array.find exact pm.segments of
      Just segment -> Just segment
      Nothing -> Array.find endpoint pm.segments
  case found of
    Nothing -> unmapped position
    Just segment -> do
      let
        r = ranges segment

        target :: SourcePos
        target =
          { line: point.line - r.from.start.line + r.to.start.line
          , column: if point.line == r.from.start.line then point.column - r.from.start.column + r.to.start.column else point.column
          }
      { valid: true, position: { line: target.line, character: offsetOf output target - lineStart output target.line } }

analyze :: String -> String -> Analysis
analyze uri text = case (if isJust (String.stripSuffix (String.Pattern ".psxt") uri) then lowerPsxt else lower) { sourceName: uri, generatedName: uri <> ".purs" } text of
  Left errors -> { valid: false, purs: "", diagnostics: errors, forward: unmapped, backward: unmapped }
  Right result -> { valid: true, purs: result.purs, diagnostics: result.warnings, forward: mapPoint text result.purs result.positionMap false, backward: mapPoint text result.purs result.positionMap true }

format :: String -> String -> Effect String
format path text = do
  config <- loadConfig path
  pure case (if isJust (String.stripSuffix (String.Pattern ".psxt") path) then formatPsxtWith config else formatPsxWith config) text of
    Left _ -> ""
    Right formatted -> formatted

-- | The same HTML/SVG data as lowering, including event attributes.
completionData :: String
completionData = stringify $ encodeJson
  { elements: Array.nub (Html.elementNames Html.Html <> Html.elementNames Html.Svg)
  , attributes: Array.nub (Html.attributeNames Html.Html <> Html.attributeNames Html.Svg <> map (\name -> maybe name _.attribute (Html.event name)) Html.eventNames <> [ "key" ])
  }

-- | Styles apply to their containing element and its descendants; top-level
-- | template styles apply to all templates. Use the CST rather than scanning
-- | HTML with a second parser.
stylesAt :: String -> String -> Position -> String
stylesAt uri text position = do
  let
    src = source text
    point = posOf src (fromMaybe 0 (Array.index src.lineStarts position.line) + position.character)
    css element = foldMap
      ( case _ of
          ChildText token -> token.text
          ChildRaw token -> token.text
          _ -> ""
      )
      element.children
    direct = foldMap
      ( case _ of
          ChildMarkup (MarkupElement element) | element.name.text == "style" -> css element
          _ -> ""
      )
    walk markup = case markup of
      MarkupElement element ->
        let
          end = maybe element.end.range.end (\close -> maybe close.name.range.end _.range.end close.end) element.close
        in
          if before point element.open.range.start || before end point then ""
          else direct element.children <> descend element.children
      MarkupFragment fragment ->
        let
          end = maybe fragment.open.range.end _.range.end fragment.close
        in
          if before point fragment.open.range.start || before end point then ""
          else direct fragment.children <> descend fragment.children
    descend = foldMap
      ( case _ of
          ChildMarkup markup -> walk markup
          _ -> ""
      )
    moduleStyles m = foldMapMarkupRoots (walk <<< _.markup) m
  if isJust (String.stripSuffix (String.Pattern ".psxt") uri) then
    maybe "" (\file -> foldMap css file.styles <> moduleStyles file.script.module <> foldMap (walk <<< MarkupElement <<< _.element) file.templates) (parsePsxt text).file
  else maybe "" moduleStyles (parseModule text).module

foreign import serve :: (String -> String -> Analysis) -> (String -> String -> Effect String) -> String -> (String -> String -> Position -> String) -> Effect Unit

main :: Effect Unit
main = serve analyze format completionData stylesAt
