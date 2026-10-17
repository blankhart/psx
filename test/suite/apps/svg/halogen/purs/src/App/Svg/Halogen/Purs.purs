-- | The svg scenario on Halogen, hand-written against Halogen's own API:
-- | Halogen has no SVG elements of its own, so they are `HH.elementNS`
-- | with SVG's namespace, and their attributes `HH.attr`. The control for
-- | `App.Svg.Halogen.Psx`.
module App.Svg.Halogen.Purs (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
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

type Fixture = { values :: Array Int, max :: Int }

type State = { values :: Array Int, max :: Int, selected :: Maybe Int }

data Action = Select Int | Grow

update :: State -> Action -> State
update state = case _ of
  Select i -> state { selected = Just i }
  Grow -> case state.selected of
    Just i -> state { values = fromMaybe state.values (Array.modifyAt i (\v -> min state.max (v + 1)) state.values) }
    Nothing -> state

-- | A bar's left edge and top, in the chart's coordinates.
left :: Int -> Int
left i = 10 + 20 * i

barTop :: Int -> Int
barTop v = 55 - 4 * v

-- | The line through the tops of the bars.
trend :: Array Int -> String
trend values = String.joinWith " " (Array.mapWithIndex point values)
  where
  point i v = (if i == 0 then "M " else "L ") <> show (left i + 8) <> " " <> show (barTop v)

view :: forall w. State -> HH.HTML w Action
view state =
  HH.figure [ HP.class_ (HH.ClassName "chart"), testId "chart" ]
    [ svg "svg" [ attr "viewBox" "0 0 120 60", attr "width" "360", attr "height" "180" ]
        ( [ HH.keyedNS ns (HH.ElemName "g") [ attr "class" "bars" ]
              ( Array.mapWithIndex
                  ( \i v ->
                      Tuple (show i) $ svg "rect"
                        [ testId ("bar-" <> show i)
                        , attr "x" (show (left i))
                        , attr "y" (show (barTop v))
                        , attr "width" "16"
                        , attr "height" (show (4 * v))
                        , attr "fill" (if state.selected == Just i then "orange" else "steelblue")
                        , HE.onClick \_ -> Select i
                        ]
                        []
                  )
                  state.values
              )
          , svg "path" [ attr "class" "trend", attr "d" (trend state.values), attr "fill" "none", attr "stroke" "gray" ] []
          , svg "line" [ attr "x1" "5", attr "y1" "55", attr "x2" "115", attr "y2" "55", attr "stroke" "black" ] []
          ]
            <> case state.selected of
              Just i -> [ svg "circle" [ attr "cx" (show (left i + 8)), attr "cy" (show (barTop (value i))), attr "r" "2", attr "fill" "orange" ] [] ]
              Nothing -> []
            <> [ svg "text" [ attr "x" "5", attr "y" "8" ] [ HH.text "Values" ] ]
        )
    , HH.figcaption [ testId "caption" ]
        [ HH.text case state.selected of
            Just i -> "Bar " <> show i <> ": " <> show (value i)
            Nothing -> "Click a bar"
        ]
    , HH.button [ testId "grow", HE.onClick \_ -> Grow, HP.disabled (state.selected == Nothing) ] [ HH.text "Grow" ]
    ]
  where
  value i = fromMaybe 0 (Array.index state.values i)

ns :: HH.Namespace
ns = HH.Namespace "http://www.w3.org/2000/svg"

svg :: forall r w i. String -> Array (HP.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svg name = HH.elementNS ns (HH.ElemName name)

attr :: forall r i. String -> String -> HP.IProp r i
attr name = HP.attr (HH.AttrName name)

testId :: forall r i. String -> HP.IProp r i
testId = attr "data-testid"

component :: forall q o m. Fixture -> H.Component q Unit o m
component fixture = H.mkComponent
  { initialState: const { values: fixture.values, max: fixture.max, selected: Nothing }
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
