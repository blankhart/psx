-- | The child: a counter component with its own state, which tells its
-- | parent its count after each change.
module App.Nested.Halogen.Purs.Counter
  ( Input
  , Output
  , counter
  ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input = { id :: Int, label :: String, start :: Int }

type State = { id :: Int, label :: String, count :: Int }

data Action = Increment | Decrement

-- | Its count, after each change.
type Output = Int

handle :: forall m. Action -> H.HalogenM State Action () Output m Unit
handle action = do
  state <- H.modify \s -> s { count = s.count + (if action == Increment then 1 else -1) }
  H.raise state.count

derive instance Eq Action

view :: forall w. State -> HH.HTML w Action
view state =
  HH.div [ HP.class_ (HH.ClassName "counter"), testId ("counter-" <> show state.id) ]
    [ HH.span [ HP.class_ (HH.ClassName "count") ] [ HH.text (state.label <> ": " <> show state.count) ]
    , HH.button [ testId ("decrement-" <> show state.id), HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.button [ testId ("increment-" <> show state.id), HE.onClick \_ -> Increment ] [ HH.text "+" ]
    ]

testId :: forall r i. String -> HP.IProp r i
testId = HP.attr (HH.AttrName "data-testid")

counter :: forall q m. H.Component q Input Output m
counter = H.mkComponent
  { initialState: \i -> { id: i.id, label: i.label, count: i.start }
  , render: view
  , eval: H.mkEval H.defaultEval { handleAction = handle }
  }
