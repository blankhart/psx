-- | The table scenario on React, hand-written against React's own API:
-- | the control for `App.Table.React.Psx`.
module App.Table.React.Purs (main) where

import Prelude

import Suite.React.Native as P

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Suite.React as React
import Suite.Shared.Table (Source, TableRow)
import Suite.Shared.Table as Table

type Model = { rows :: Array TableRow, selected :: Maybe Int, source :: Source }

data Msg = Create Int | Append Int | Update | Clear | Swap | Select Int | Remove Int

update :: Model -> Msg -> Model
update model = case _ of
  Create n -> let made = Table.rows n model.source in model { rows = made.rows, source = made.source }
  Append n -> let made = Table.rows n model.source in model { rows = model.rows <> made.rows, source = made.source }
  Update -> model { rows = Table.updateEvery model.rows }
  Clear -> model { rows = [] }
  Swap -> model { rows = Table.swap model.rows }
  Select id -> model { selected = Just id }
  Remove id -> model { rows = Array.filter (\row -> row.id /= id) model.rows }

buttons :: Array { id :: String, text :: String, msg :: Msg }
buttons =
  [ { id: "run", text: "Create 1,000 rows", msg: Create 1000 }
  , { id: "runlots", text: "Create 10,000 rows", msg: Create 10000 }
  , { id: "add", text: "Append 1,000 rows", msg: Append 1000 }
  , { id: "update", text: "Update every 10th row", msg: Update }
  , { id: "clear", text: "Clear", msg: Clear }
  , { id: "swaprows", text: "Swap Rows", msg: Swap }
  ]

view :: (Msg -> Effect Unit) -> Model -> P.Html
view dispatch model =
  P.div [ P.class' "container", testId "table" ]
    [ P.div [ P.class' "jumbotron" ]
        [ P.div [ P.class' "row" ]
            [ P.div [ P.class' "col-md-6" ] [ P.h1 [] [ P.text "PSX table" ] ]
            , P.div [ P.class' "col-md-6" ]
                ( map
                    ( \b ->
                        P.div [ P.class' "col-sm-6 smallpad" ]
                          [ P.button [ P.type' "button", P.class' "btn btn-primary btn-block", P.id b.id, testId b.id, P.onClick (dispatch b.msg) ]
                              [ P.text b.text ]
                          ]
                    )
                    buttons
                )
            ]
        ]
    , P.table [ P.class' "table table-hover table-striped test-data" ]
        [ P.tbody []
            ( map
                ( \row ->
                    P.tr [ P.key (show row.id), P.class' (if model.selected == Just row.id then "danger" else "row") ]
                      [ P.td [ P.class' "col-md-1" ] [ P.text (show row.id) ]
                      , P.td [ P.class' "col-md-4" ]
                          [ P.a [ testId ("select-" <> show row.id), P.onClick (dispatch ((Select row.id))) ] [ P.text row.label ] ]
                      , P.td [ P.class' "col-md-1" ]
                          [ P.a [ testId ("remove-" <> show row.id), P.onClick (dispatch ((Remove row.id))) ]
                              [ P.span [ P.class' "glyphicon glyphicon-remove", P.attribute "aria-hidden" "true" ] [] ]
                          ]
                      , P.td [ P.class' "col-md-6" ] []
                      ]
                )
                model.rows
            )
        ]
    , P.span [ P.class' "preloadicon glyphicon glyphicon-remove", P.attribute "aria-hidden" "true" ] []
    ]

testId :: String -> P.Trait
testId = P.attribute "data-testid"

main :: { seed :: Number } -> Effect Unit
main fixture = React.mount ({ rows: [], selected: Nothing, source: Table.source fixture.seed }) update (\dispatch model -> view dispatch model)
