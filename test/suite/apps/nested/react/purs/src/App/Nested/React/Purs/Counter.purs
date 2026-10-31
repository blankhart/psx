-- | The child: a counter with its own model, messages and view, which the
-- | parent composes.
module App.Nested.React.Purs.Counter
  ( Model
  , Msg
  , update
  , view
  ) where

import Prelude
import Effect (Effect)

import Suite.React.Native as P
import Suite.React.Native as S

type Model = Int

data Msg = Increment | Decrement

update :: Model -> Msg -> Model
update n = case _ of
  Increment -> n + 1
  Decrement -> n - 1

view :: (Msg -> Effect Unit) -> Int -> String -> Model -> P.Html
view dispatch id label n =
  P.div [ P.class' "counter", testId ("counter-" <> show id) ]
    [ P.span [ P.class' "count" ] [ P.text (label <> ": " <> show n) ]
    , P.button [ testId ("decrement-" <> show id), P.onClick (dispatch (Decrement)) ] [ P.text "-" ]
    , P.button [ testId ("increment-" <> show id), P.onClick (dispatch (Increment)) ] [ P.text "+" ]
    ]

testId :: String -> P.Trait
testId = P.attribute "data-testid"
