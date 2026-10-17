-- | The counter scenario on Halogen, hand-written against Halogen's own API:
-- | the control for `App.Counter.Halogen.Psx`.
module App.Counter.Halogen.Purs (main) where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM.Node (removeChild)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML.HTMLElement (toNode)

type Props = { label :: String, max :: Int }

data Msg = Increment | Decrement | Reset

update :: Int -> Msg -> Int
update n = case _ of
  Increment -> n + 1
  Decrement -> n - 1
  Reset -> 0

view :: forall w. Props -> Int -> HH.HTML w Msg
view props n =
  HH.section [ HP.class_ (HH.ClassName "counter"), testId "counter" ]
    ( [ HH.h2 [ testId "label" ] [ HH.text (props.label <> ": " <> show n) ]
      , HH.button [ testId "decrement", HE.onClick \_ -> Decrement, HP.disabled (n <= 0) ] [ HH.text "-" ]
      , HH.button [ testId "increment", HE.onClick \_ -> Increment, HP.disabled (n >= props.max) ] [ HH.text "+" ]
      ]
        <> (if n == 0 then [] else [ HH.button [ testId "reset", HE.onClick \_ -> Reset ] [ HH.text "Reset" ] ])
        <>
          [ HH.keyed (HH.ElemName "ol") [ HP.class_ (HH.ClassName "pips") ]
              ( map
                  (\i -> Tuple (show i) (HH.li [ HP.class_ (HH.ClassName (if i <= n then "pip filled" else "pip")) ] []))
                  (1 .. props.max)
              )
          ]
    )

testId :: forall r i. String -> HP.IProp r i
testId = HP.attr (HH.AttrName "data-testid")

component :: forall q o m. Props -> H.Component q Unit o m
component props = H.mkComponent
  { initialState: const 0
  , render: view props
  , eval: H.mkEval H.defaultEval { handleAction = \msg -> H.modify_ (\n -> update n msg) }
  }

-- | Mounts in place of `#app`, with the scenario's fixture as the props.
main :: Props -> Effect Unit
main props = HA.runHalogenAff do
  app <- HA.selectElement (QuerySelector "#app")
  parent <- HA.selectElement (QuerySelector "#root")
  for_ parent \p -> do
    for_ app \a -> liftEffect (removeChild (toNode a) (toNode p))
    runUI (component props) unit p
