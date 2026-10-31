-- | The nested scenario on React, hand-written against React's own API:
-- | the control for `App.Nested.React.Psx`.
module App.Nested.React.Purs (main) where

import Prelude

import Suite.React.Native as P

import Data.Array as Array
import Data.Foldable (foldl, sum)
import Effect (Effect)
import Suite.React as React
import App.Nested.React.Purs.Counter as Counter

type Child = { id :: Int, label :: String, count :: Counter.Model }

type Fixture = { counters :: Array { id :: Int, label :: String, start :: Int } }

type Model = { children :: Array Child, next :: Int }

data Msg = Add | Remove Int | Child Int Counter.Msg

init :: Fixture -> Model
init fixture =
  { children: map (\c -> { id: c.id, label: c.label, count: c.start }) fixture.counters
  , next: 1 + foldl (\n c -> max n c.id) 0 fixture.counters
  }

update :: Model -> Msg -> Model
update model = case _ of
  Add ->
    model
      { children = Array.snoc model.children { id: model.next, label: "Counter " <> show model.next, count: 0 }
      , next = model.next + 1
      }
  Remove id -> model { children = Array.filter (\c -> c.id /= id) model.children }
  Child id msg -> model { children = map (\c -> if c.id == id then c { count = Counter.update c.count msg } else c) model.children }

view :: (Msg -> Effect Unit) -> Model -> P.Html
view dispatch model =
  P.section [ P.class' "nested", testId "nested" ]
    [ P.p [ testId "total" ] [ P.text ("Total: " <> show (sum (map _.count model.children))) ]
    , P.ul [ P.class' "counters" ]
        ( map
            ( \c ->
                P.li [ P.key (show c.id), P.class' "child" ]
                  [ Counter.view (dispatch <<< Child c.id) c.id c.label c.count
                  , P.button [ testId ("remove-" <> show c.id), P.onClick (dispatch ((Remove c.id))) ] [ P.text "Remove" ]
                  ]
            )
            model.children
        )
    , P.button [ testId "add", P.onClick (dispatch (Add)) ] [ P.text "Add a counter" ]
    ]

testId :: String -> P.Trait
testId = P.attribute "data-testid"

main :: Fixture -> Effect Unit
main fixture = React.mount (init fixture) update (\dispatch model -> view dispatch model)
