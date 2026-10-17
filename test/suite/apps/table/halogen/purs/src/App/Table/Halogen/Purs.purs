-- | The table scenario on Halogen, hand-written against Halogen's own API:
-- | the control for `App.Table.Halogen.Psx`.
module App.Table.Halogen.Purs (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.Node (removeChild)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (toNode)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Suite.Shared.Table (Source, TableRow)
import Suite.Shared.Table as Table

type State = { rows :: Array TableRow, selected :: Maybe Int, source :: Source }

data Action = Create Int | Append Int | Update | Clear | Swap | Select Int | Remove Int

update :: State -> Action -> State
update state = case _ of
  Create n -> let made = Table.rows n state.source in state { rows = made.rows, source = made.source }
  Append n -> let made = Table.rows n state.source in state { rows = state.rows <> made.rows, source = made.source }
  Update -> state { rows = Table.updateEvery state.rows }
  Clear -> state { rows = [] }
  Swap -> state { rows = Table.swap state.rows }
  Select id -> state { selected = Just id }
  Remove id -> state { rows = Array.filter (\row -> row.id /= id) state.rows }

buttons :: Array { id :: String, text :: String, action :: Action }
buttons =
  [ { id: "run", text: "Create 1,000 rows", action: Create 1000 }
  , { id: "runlots", text: "Create 10,000 rows", action: Create 10000 }
  , { id: "add", text: "Append 1,000 rows", action: Append 1000 }
  , { id: "update", text: "Update every 10th row", action: Update }
  , { id: "clear", text: "Clear", action: Clear }
  , { id: "swaprows", text: "Swap Rows", action: Swap }
  ]

view :: forall w. State -> HH.HTML w Action
view state =
  HH.div [ class_ "container", testId "table" ]
    [ HH.div [ class_ "jumbotron" ]
        [ HH.div [ class_ "row" ]
            [ HH.div [ class_ "col-md-6" ] [ HH.h1_ [ HH.text "PSX table" ] ]
            , HH.div [ class_ "col-md-6" ]
                ( map
                    ( \b ->
                        HH.div [ class_ "col-sm-6 smallpad" ]
                          [ HH.button [ HP.type_ HP.ButtonButton, class_ "btn btn-primary btn-block", HP.id b.id, testId b.id, HE.onClick \_ -> b.action ]
                              [ HH.text b.text ]
                          ]
                    )
                    buttons
                )
            ]
        ]
    , HH.table [ class_ "table table-hover table-striped test-data" ]
        [ HH.keyed (HH.ElemName "tbody") []
            ( map
                ( \row ->
                    Tuple (show row.id) $ HH.tr [ class_ (if state.selected == Just row.id then "danger" else "row") ]
                      [ HH.td [ class_ "col-md-1" ] [ HH.text (show row.id) ]
                      , HH.td [ class_ "col-md-4" ]
                          [ HH.a [ testId ("select-" <> show row.id), HE.onClick \_ -> Select row.id ] [ HH.text row.label ] ]
                      , HH.td [ class_ "col-md-1" ]
                          [ HH.a [ testId ("remove-" <> show row.id), HE.onClick \_ -> Remove row.id ]
                              [ HH.span [ class_ "glyphicon glyphicon-remove", HP.attr (HH.AttrName "aria-hidden") "true" ] [] ]
                          ]
                      , HH.td [ class_ "col-md-6" ] []
                      ]
                )
                state.rows
            )
        ]
    , HH.span [ class_ "preloadicon glyphicon glyphicon-remove", HP.attr (HH.AttrName "aria-hidden") "true" ] []
    ]

class_ :: forall r i. String -> HP.IProp (class :: String | r) i
class_ = HP.class_ <<< HH.ClassName

testId :: forall r i. String -> HP.IProp r i
testId = HP.attr (HH.AttrName "data-testid")

component :: forall q o m. Number -> H.Component q Unit o m
component seed = H.mkComponent
  { initialState: const { rows: [], selected: Nothing, source: Table.source seed }
  , render: view
  , eval: H.mkEval H.defaultEval { handleAction = \action -> H.modify_ (\state -> update state action) }
  }

-- | Mounts in place of `#app`, with the scenario's seed.
main :: { seed :: Number } -> Effect Unit
main fixture = HA.runHalogenAff do
  app <- HA.selectElement (QuerySelector "#app")
  parent <- HA.selectElement (QuerySelector "#root")
  for_ parent \p -> do
    for_ app \a -> liftEffect (removeChild (toNode a) (toNode p))
    runUI (component fixture.seed) unit p
