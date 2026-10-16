-- | The counter scenario on Portico, hand-written against Portico's own API:
-- | the control for `App.Counter.Portico.Psx`.
module App.Counter.Portico.Purs (main) where

import Prelude

import Data.Array ((..))
import Effect (Effect)
import Portico ((:>))
import Portico as P

type Props = { label :: String, max :: Int }

data Msg = Increment | Decrement | Reset

update :: Int -> Msg -> Int
update n = case _ of
  Increment -> n + 1
  Decrement -> n - 1
  Reset -> 0

view :: Props -> Int -> P.Html Msg
view props n =
  P.section [ P.class' "counter", testId "counter" ]
    ( [ P.h2 [ testId "label" ] [ P.text (props.label <> ": " <> show n) ]
      , P.button [ testId "decrement", P.onClick Decrement, P.disabled (n <= 0) ] [ P.text "-" ]
      , P.button [ testId "increment", P.onClick Increment, P.disabled (n >= props.max) ] [ P.text "+" ]
      ]
        <> (if n == 0 then [] else [ P.button [ testId "reset", P.onClick Reset ] [ P.text "Reset" ] ])
        <>
          [ P.ol [ P.class' "pips" ]
              ( map
                  (\i -> P.li [ P.key (show i), P.class' (if i <= n then "pip filled" else "pip") ] [])
                  (1 .. props.max)
              )
          ]
    )

testId :: forall msg. String -> P.Trait msg
testId = P.attribute "data-testid"

-- | Mounts on `#app`, with the scenario's fixture as the props.
main :: Props -> Effect Unit
main props =
  void $ P.mount "#app"
    { init: 0 :> []
    , update: \n msg -> update n msg :> []
    , subscriptions: P.noSubscriptions
    , view: view props
    }
