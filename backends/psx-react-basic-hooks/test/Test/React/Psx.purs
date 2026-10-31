module Test.React.Psx (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Exception (throw)
import React.Basic (JSX)
import React.Basic.Hooks.Psx as B

foreign import render :: JSX -> String
foreign import keyOf :: JSX -> String
foreign import invokeClick :: JSX -> Effect Unit

assert :: String -> Boolean -> Effect Unit
assert label ok = unless ok (throw label)

main :: Effect Unit
main = do
  assert "attributes and escaping" $ render (B.div [ B.class_ "test", B.attribute "data-x" "a&b" ] [ B.text "<hello>" ]) == "<div class=\"test\" data-x=\"a&amp;b\">&lt;hello&gt;</div>"
  assert "false boolean" $ render (B.button [ B.disabled false ] B.none) == "<button></button>"
  assert "fragment" $ render (B.root [ B.text "a", B.span [] [ B.text "b" ] ]) == "a<span>b</span>"
  assert "key belongs to subtree" $ keyOf (B.ul [] [ B.keyed 7 (B.li [] B.none) ]) == "7"
  calls <- Ref.new 0
  invokeClick (B.button [ B.onClick (Ref.modify_ (_ + 1) calls) ] B.none)
  actual <- Ref.read calls
  assert "event dispatches its effect once" (actual == 1)
  log "React PSX adapter tests passed"
