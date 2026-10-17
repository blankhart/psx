-- | The nested scenario on Halogen, hand-written against Halogen's own API:
-- | the control for `App.Nested.Halogen.Psx`.
module App.Nested.Halogen.Purs (main) where

import Prelude

import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (foldl, for_, sum)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Type.Proxy (Proxy(..))
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
import App.Nested.Halogen.Purs.Counter as Counter

type Fixture = { counters :: Array Counter.Input }

type State =
  { children :: Array Counter.Input
  -- | What each child last reported.
  , counts :: Map Int Int
  , next :: Int
  }

data Action = Add | Remove Int | Changed Int Int

type Slots = (counter :: H.Slot (Const Void) Counter.Output Int)

_counter :: Proxy "counter"
_counter = Proxy

initial :: Fixture -> State
initial fixture =
  { children: fixture.counters
  , counts: Map.fromFoldable (map (\c -> Tuple c.id c.start) fixture.counters)
  , next: 1 + foldl (\n c -> max n c.id) 0 fixture.counters
  }

update :: State -> Action -> State
update state = case _ of
  Add ->
    state
      { children = Array.snoc state.children { id: state.next, label: "Counter " <> show state.next, start: 0 }
      , counts = Map.insert state.next 0 state.counts
      , next = state.next + 1
      }
  Remove id -> state { children = Array.filter (\c -> c.id /= id) state.children, counts = Map.delete id state.counts }
  Changed id n -> state { counts = Map.insert id n state.counts }

view :: forall m. State -> H.ComponentHTML Action Slots m
view state =
  HH.section [ HP.class_ (HH.ClassName "nested"), testId "nested" ]
    [ HH.p [ testId "total" ] [ HH.text ("Total: " <> show (sum state.counts)) ]
    , HH.keyed (HH.ElemName "ul") [ HP.class_ (HH.ClassName "counters") ]
        ( map
            ( \c ->
                Tuple (show c.id) $ HH.li [ HP.class_ (HH.ClassName "child") ]
                  [ HH.slot _counter c.id Counter.counter c (Changed c.id)
                  , HH.button [ testId ("remove-" <> show c.id), HE.onClick \_ -> Remove c.id ] [ HH.text "Remove" ]
                  ]
            )
            state.children
        )
    , HH.button [ testId "add", HE.onClick \_ -> Add ] [ HH.text "Add a counter" ]
    ]

testId :: forall r i. String -> HP.IProp r i
testId = HP.attr (HH.AttrName "data-testid")

component :: forall q o m. Fixture -> H.Component q Unit o m
component fixture = H.mkComponent
  { initialState: const (initial fixture)
  , render: view
  , eval: H.mkEval H.defaultEval { handleAction = \action -> H.modify_ (\state -> update state action) }
  }

-- | Mounts in place of `#app`, with the scenario's fixture.
main :: Fixture -> Effect Unit
main fixture = HA.runHalogenAff do
  app <- HA.selectElement (QuerySelector "#app")
  parent <- HA.selectElement (QuerySelector "#root")
  for_ parent \p -> do
    for_ app \a -> liftEffect (removeChild (toNode a) (toNode p))
    runUI (component fixture) unit p
