-- | The typeahead scenario on Halogen, hand-written against Halogen's own
-- | API: the control for `App.Typeahead.Halogen.Psx`.
module App.Typeahead.Halogen.Purs (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
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
import Suite.Shared.Typeahead (Fixture, search)
import Web.UIEvent.KeyboardEvent as KE

type State =
  { query :: String
  -- | The query the results answer.
  , answered :: String
  , results :: Array String
  , selected :: Int
  , chosen :: String
  -- | The search waiting for its answer.
  , pending :: Maybe H.ForkId
  }

data Action = Query String | Key String

initial :: State
initial = { query: "", answered: "", results: [], selected: 0, chosen: "", pending: Nothing }

-- | Each query is forked, and a newer one kills the one still waiting.
handle :: forall o m. MonadAff m => Fixture -> Action -> H.HalogenM State Action () o m Unit
handle fixture = case _ of
  Query query -> do
    H.gets _.pending >>= traverse_ H.kill
    if String.length query < 3 then
      H.modify_ _ { query = query, answered = query, results = [], pending = Nothing }
    else do
      H.modify_ _ { query = query }
      pending <- H.fork do
        H.liftAff (delay (Milliseconds fixture.debounce))
        results <- H.liftAff (search fixture query)
        H.modify_ _ { answered = query, results = results, selected = 0, pending = Nothing }
      H.modify_ _ { pending = Just pending }
  Key key -> case key of
    "ArrowDown" -> move 1
    "ArrowUp" -> move (-1)
    "Enter" -> H.modify_ \s -> s { chosen = fromMaybe s.chosen (Array.index s.results s.selected) }
    _ -> pure unit
  where
  move by = H.modify_ \s -> s { selected = clamp 0 (Array.length s.results - 1) (s.selected + by) }

status :: State -> String
status state
  | String.length state.query < 3 = "Type at least three letters"
  | state.answered /= state.query = "Searching…"
  | Array.length state.results == 1 = "1 result"
  | otherwise = show (Array.length state.results) <> " results"

view :: forall w. State -> HH.HTML w Action
view state =
  HH.div [ HP.class_ (HH.ClassName "typeahead"), testId "typeahead" ]
    [ HH.input [ HP.type_ HP.InputText, testId "search", HP.placeholder "Search fruit", HE.onValueInput Query, HE.onKeyDown (Key <<< KE.key) ]
    , HH.p [ testId "status" ] [ HH.text (status state) ]
    , HH.keyed (HH.ElemName "ul") [ HP.class_ (HH.ClassName "results") ]
        ( Array.mapWithIndex
            ( \i name ->
                Tuple name $ HH.li
                  [ testId ("result-" <> show i), HP.class_ (HH.ClassName (if i == state.selected then "result selected" else "result")) ]
                  [ HH.text name ]
            )
            state.results
        )
    , HH.p [ testId "chosen" ] [ HH.text (if state.chosen == "" then "" else "Chosen: " <> state.chosen) ]
    ]

testId :: forall r i. String -> HP.IProp r i
testId = HP.attr (HH.AttrName "data-testid")

component :: forall q o m. MonadAff m => Fixture -> H.Component q Unit o m
component fixture = H.mkComponent
  { initialState: const initial
  , render: view
  , eval: H.mkEval H.defaultEval { handleAction = handle fixture }
  }

-- | Mounts in place of `#app`, with the scenario's fixture.
main :: Fixture -> Effect Unit
main fixture = HA.runHalogenAff do
  app <- HA.selectElement (QuerySelector "#app")
  parent <- HA.selectElement (QuerySelector "#root")
  for_ parent \p -> do
    for_ app \a -> liftEffect (removeChild (toNode a) (toNode p))
    runUI (component fixture) unit p
