-- | The typeahead scenario on Portico, hand-written against Portico's own
-- | API: the control for `App.Typeahead.Portico.Psx`.
module App.Typeahead.Portico.Purs (main) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Portico ((:>))
import Portico as P
import Suite.Shared.Typeahead (Fixture, search)

type Model =
  { query :: String
  -- | The query the results answer.
  , answered :: String
  , results :: Array String
  , selected :: Int
  , chosen :: String
  }

data Msg = Query String | Answer String (Array String) | Move Int | Choose

init :: Model
init = { query: "", answered: "", results: [], selected: 0, chosen: "" }

update :: Fixture -> Model -> Msg -> Tuple Model (Array (P.Sub Msg))
update fixture model = case _ of
  Query query
    | String.length query < 3 ->
        Tuple model { query = query, answered = query, results = [] } [ P.latest "search" (P.runAff (pure unit)) ]
    | otherwise ->
        Tuple model { query = query }
          [ P.latest "search" \sink -> do
              delay (Milliseconds fixture.debounce)
              results <- search fixture query
              liftEffect (sink (Answer query results))
          ]
  Answer query results
    | query == model.query -> model { answered = query, results = results, selected = 0 } :> []
    | otherwise -> model :> []
  Move by -> model { selected = clamp 0 (Array.length model.results - 1) (model.selected + by) } :> []
  Choose -> model { chosen = fromMaybe model.chosen (Array.index model.results model.selected) } :> []

key :: String -> Maybe Msg
key = case _ of
  "ArrowDown" -> Just (Move 1)
  "ArrowUp" -> Just (Move (-1))
  "Enter" -> Just Choose
  _ -> Nothing

status :: Model -> String
status model
  | String.length model.query < 3 = "Type at least three letters"
  | model.answered /= model.query = "Searching…"
  | Array.length model.results == 1 = "1 result"
  | otherwise = show (Array.length model.results) <> " results"

view :: Model -> P.Html Msg
view model =
  P.div [ P.class' "typeahead", testId "typeahead" ]
    [ P.input [ P.type' "text", testId "search", P.placeholder "Search fruit", P.onInput Query, P.onKeyDown key ] []
    , P.p [ testId "status" ] [ P.text (status model) ]
    , P.ul [ P.class' "results" ]
        ( Array.mapWithIndex
            ( \i name ->
                P.li [ P.key name, testId ("result-" <> show i), P.class' (if i == model.selected then "result selected" else "result") ]
                  [ P.text name ]
            )
            model.results
        )
    , P.p [ testId "chosen" ] [ P.text (if model.chosen == "" then "" else "Chosen: " <> model.chosen) ]
    ]

testId :: forall msg. String -> P.Trait msg
testId = P.attribute "data-testid"

-- | Mounts on `#app`, with the scenario's fixture as the server.
main :: Fixture -> Effect Unit
main fixture =
  void $ P.mount "#app"
    { init: init :> []
    , update: update fixture
    , subscriptions: P.noSubscriptions
    , view
    }
