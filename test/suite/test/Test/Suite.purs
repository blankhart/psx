module Test.Suite where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Suite.Dom (Node(..), normalise, print)

-- | The normaliser joins split text, drops empty text and sorts attributes.
main :: Effect Unit
main = do
  let
    actual = print $ normalise
      [ Element "p" [ Tuple "id" "x", Tuple "class" "a" ]
          [ Text "Clicks", Text "", Text ": ", Text "0" ]
      , Element "input" [ Tuple "value" "say \"hi\"", Tuple "disabled" "" ] []
      , Element "ul" [] []
      ]
    expected =
      "<p class=\"a\" id=\"x\">\n  \"Clicks: 0\"\n</p>\n<input disabled value=\"say &quot;hi&quot;\">\n<ul></ul>\n"
  when (actual /= expected) $ throw $ "normalise and print gave\n" <> actual
