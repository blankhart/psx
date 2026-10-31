-- | The typeahead scenario on React, hand-written against React's own
-- | API: the control for `App.Typeahead.React.Psx`.
module App.Typeahead.React.Purs (main) where

import Prelude

import Suite.React.Native as P

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Suite.React as React
import Effect.Aff (delay, launchAff)
import Effect.Class (liftEffect)
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

update :: Fixture -> ((Model -> Model) -> Effect Unit) -> Model -> Msg -> Effect Unit
update fixture set _ = case _ of
  Query query -> do
    set (_ { query = query })
    if String.length query < 3 then
      set (_ { answered = query, results = [] })
    else void $ launchAff do
      delay (Milliseconds fixture.debounce)
      results <- search fixture query
      liftEffect $ set \current ->
        if current.query == query then current { answered = query, results = results, selected = 0 }
        else current
  Answer query results -> set \current ->
    if current.query == query then current { answered = query, results = results, selected = 0 } else current
  Move by -> set (\current -> current { selected = clamp 0 (Array.length current.results - 1) (current.selected + by) })
  Choose -> set (\current -> current { chosen = fromMaybe current.chosen (Array.index current.results current.selected) })

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

view :: (Msg -> Effect Unit) -> Model -> P.Html
view dispatch model =
  P.div [ P.class' "typeahead", testId "typeahead" ]
    [ P.input [ P.type' "text", testId "search", P.placeholder "Search fruit", P.onInput (dispatch <<< Query), P.onKeyDown (map dispatch <<< key) ] []
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

testId :: String -> P.Trait
testId = P.attribute "data-testid"

main :: Fixture -> Effect Unit
main fixture = React.mountWith init (update fixture) (\dispatch model -> view dispatch model)
