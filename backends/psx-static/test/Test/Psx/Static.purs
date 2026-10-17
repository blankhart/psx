module Test.Psx.Static where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Psx.Static as B

main :: Effect Unit
main = do
  rendering
  roots
  log "psx-static: all tests passed"

assert :: String -> String -> String -> Effect Unit
assert what expected actual =
  unless (expected == actual) (throw ("FAIL " <> what <> ": expected " <> show expected <> ", got " <> show actual))

-- What lowered code builds renders as HTML.
rendering :: Effect Unit
rendering = do
  assert "an element with attributes and text"
    "<button class=\"go\" disabled>Go &amp; see</button>"
    (B.render (B.button [ B.class_ "go", B.disabled true, B.onClick unit ] (B.children "Go & see")))
  assert "a false boolean and a handler render nothing"
    "<input type=\"text\" value=\"a &quot;b&quot;\">"
    (B.render (B.input [ B.type' "text", B.required false, B.value "a \"b\"", B.onInput (const unit) ] B.none))
  assert "numbers and custom attributes"
    "<td colspan=\"2\" data-x=\"<y>\">&lt;z&gt;</td>"
    (B.render (B.td [ B.colspan 2, B.attribute "data-x" "<y>" ] [ B.text "<z>" ] :: B.Node Unit))
  assert "keys are dropped"
    "<ul><li>a</li><li>b</li></ul>"
    (B.render (B.ul [] [ B.keyed 1 (B.li [] [ B.text "a" ]), B.keyed 2 (B.li [] [ B.text "b" ]) ] :: B.Node Unit))
  assert "script and style text is raw"
    "<style>a > b { color: red }</style>"
    (B.render (B.style [] [ B.text "a > b { color: red }" ] :: B.Node Unit))
  assert "SVG keeps its case"
    "<svg viewBox=\"0 0 1 1\"><textPath href=\"#p\"></textPath></svg>"
    (B.render (B.svg [ B.viewBox "0 0 1 1" ] [ B.textPath [ B.href "#p" ] B.none ] :: B.Node Unit))

-- `root` is the only node, or a fragment of several.
roots :: Effect Unit
roots = do
  assert "one node" "<p></p>" (B.render (B.root [ B.p [] B.none ] :: B.Node Unit))
  assert "several, one after another" "a<p></p>" (B.render (B.root (B.children "a" <> [ B.p [] B.none ]) :: B.Node Unit))
  assert "markup" "<br><hr>" (B.renderMarkup [ B.br [] B.none, B.hr [] B.none ] :: String)
