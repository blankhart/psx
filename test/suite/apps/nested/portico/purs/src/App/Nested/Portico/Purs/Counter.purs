-- | The child: a counter with its own model, messages and view, which the
-- | parent composes.
module App.Nested.Portico.Purs.Counter
  ( Model
  , Msg
  , update
  , view
  ) where

import Prelude

import Portico as P

type Model = Int

data Msg = Increment | Decrement

update :: Model -> Msg -> Model
update n = case _ of
  Increment -> n + 1
  Decrement -> n - 1

view :: Int -> String -> Model -> P.Html Msg
view id label n =
  P.div [ P.class' "counter", testId ("counter-" <> show id) ]
    [ P.span [ P.class' "count" ] [ P.text (label <> ": " <> show n) ]
    , P.button [ testId ("decrement-" <> show id), P.onClick Decrement ] [ P.text "-" ]
    , P.button [ testId ("increment-" <> show id), P.onClick Increment ] [ P.text "+" ]
    ]

testId :: forall msg. String -> P.Trait msg
testId = P.attribute "data-testid"
