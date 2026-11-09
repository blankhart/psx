module Test.Psx.LanguageServer where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Psx.LanguageServer (analyze, format, stylesAt)
import Data.String as String

main :: Effect Unit
main = do
  let input = "module Test where\n\nimport Psx.Static\n\nview = <div>{\"é😀\"}</div>\n"
  unless (analyze "test.psx" input).valid (throw "valid buffer did not lower")
  when (analyze "test.psx" "x = <div>").valid (throw "invalid buffer accepted")
  when (format "test.psx" input == "") (throw "valid buffer did not format")
  let
    scoped = "module Test where\n\nimport Psx.Static\n\nview = <div><style>.here { color: red }</style><i class=\"here\"/></div>\nother = <div><style>.elsewhere { color: blue }</style></div>\n"
    css = stylesAt "test.psx" scoped { line: 4, character: 55 }
  unless (String.contains (String.Pattern ".here") css) (throw "missing ancestor style")
  when (String.contains (String.Pattern ".elsewhere") css) (throw "sibling style leaked")
  let
    lowered = analyze "test.psx" input
    point = { line: 4, character: 14 }
    generated = lowered.forward point
    restored = lowered.backward generated.position
  unless (generated.valid && restored.valid && restored.position == point) (throw "hole position did not round trip")
  log "language-server analysis passed (stdio gate: npm run test:language-server)"
