-- | The counter scenario on React, hand-written against React's own API:
-- | the control for `App.Counter.React.Psx`.
module App.Counter.React.Purs (main) where

import Prelude

import Suite.React.Native as P

import Data.Array ((..))
import Effect (Effect)
import Suite.React as React

type Props = { label :: String, max :: Int }

data Msg = Increment | Decrement | Reset

update :: Int -> Msg -> Int
update n = case _ of
  Increment -> n + 1
  Decrement -> n - 1
  Reset -> 0

view :: (Msg -> Effect Unit) -> Props -> Int -> P.Html
view dispatch props n =
  P.section [ P.class' "counter", testId "counter" ]
    ( [ P.h2 [ testId "label" ] [ P.text (props.label <> ": " <> show n) ]
      , P.button [ testId "decrement", P.onClick (dispatch (Decrement)), P.disabled (n <= 0) ] [ P.text "-" ]
      , P.button [ testId "increment", P.onClick (dispatch (Increment)), P.disabled (n >= props.max) ] [ P.text "+" ]
      ]
        <> (if n == 0 then [] else [ P.button [ testId "reset", P.onClick (dispatch (Reset)) ] [ P.text "Reset" ] ])
        <>
          [ P.ol [ P.class' "pips" ]
              ( map
                  (\i -> P.li [ P.key (show i), P.class' (if i <= n then "pip filled" else "pip") ] [])
                  (1 .. props.max)
              )
          ]
    )

testId :: String -> P.Trait
testId = P.attribute "data-testid"

main :: Props -> Effect Unit
main fixture = React.mount (0) update (\dispatch model -> view dispatch fixture model)
