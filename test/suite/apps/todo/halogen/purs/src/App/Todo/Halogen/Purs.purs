-- | The todo scenario on Halogen, hand-written against Halogen's own API:
-- | the control for `App.Todo.Halogen.Psx`.
module App.Todo.Halogen.Purs (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
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

type Todo = { id :: Int, text :: String, done :: Boolean }

type Fixture = { todos :: Array Todo }

data Filter = All | Active | Completed

derive instance Eq Filter

type Model = { todos :: Array Todo, draft :: String, filter :: Filter, next :: Int }

data Msg = Draft String | Add | Toggle Int | Remove Int | Show Filter

init :: Fixture -> Model
init fixture =
  { todos: fixture.todos
  , draft: ""
  , filter: All
  , next: 1 + foldl (\n todo -> max n todo.id) 0 fixture.todos
  }

update :: Model -> Msg -> Model
update model = case _ of
  Draft draft -> model { draft = draft }
  Add ->
    model
      { todos = Array.snoc model.todos { id: model.next, text: model.draft, done: false }
      , draft = ""
      , next = model.next + 1
      }
  Toggle id -> model { todos = map (\todo -> if todo.id == id then todo { done = not todo.done } else todo) model.todos }
  Remove id -> model { todos = Array.filter (\todo -> todo.id /= id) model.todos }
  Show filter -> model { filter = filter }

shown :: Filter -> Todo -> Boolean
shown filter todo = case filter of
  All -> true
  Active -> not todo.done
  Completed -> todo.done

view :: forall w. Model -> HH.HTML w Msg
view model =
  HH.section [ HP.class_ (HH.ClassName "todo"), testId "todo" ]
    [ HH.h1_ [ HH.text "Todos" ]
    , HH.div [ HP.class_ (HH.ClassName "new") ]
        [ HH.input [ HP.type_ HP.InputText, testId "draft", HP.value model.draft, HE.onValueInput Draft ]
        , HH.button [ testId "add", HE.onClick \_ -> Add, HP.disabled (model.draft == "") ] [ HH.text "Add" ]
        ]
    , HH.keyed (HH.ElemName "ul") [ HP.class_ (HH.ClassName "items") ]
        (map item (Array.filter (shown model.filter) model.todos))
    , HH.p [ testId "left" ] [ HH.text (show (Array.length (Array.filter (not <<< _.done) model.todos)) <> " left") ]
    , HH.div [ HP.class_ (HH.ClassName "filters") ]
        [ HH.button [ testId "all", HE.onClick \_ -> Show All, HP.disabled (model.filter == All) ] [ HH.text "All" ]
        , HH.button [ testId "active", HE.onClick \_ -> Show Active, HP.disabled (model.filter == Active) ] [ HH.text "Active" ]
        , HH.button [ testId "completed", HE.onClick \_ -> Show Completed, HP.disabled (model.filter == Completed) ] [ HH.text "Completed" ]
        ]
    ]
  where
  item todo =
    Tuple (show todo.id)
      $ HH.li [ HP.class_ (HH.ClassName (if todo.done then "item done" else "item")) ]
          [ HH.input [ HP.type_ HP.InputCheckbox, testId ("toggle-" <> show todo.id), HP.checked todo.done, HE.onChange \_ -> Toggle todo.id ]
          , HH.span_ [ HH.text todo.text ]
          , HH.button [ testId ("remove-" <> show todo.id), HE.onClick \_ -> Remove todo.id ] [ HH.text "Remove" ]
          ]

testId :: forall r i. String -> HP.IProp r i
testId = HP.attr (HH.AttrName "data-testid")

component :: forall q o m. Fixture -> H.Component q Unit o m
component fixture = H.mkComponent
  { initialState: const (init fixture)
  , render: view
  , eval: H.mkEval H.defaultEval { handleAction = \msg -> H.modify_ (\model -> update model msg) }
  }

-- | Mounts in place of `#app`, starting from the scenario's fixture.
main :: Fixture -> Effect Unit
main fixture = HA.runHalogenAff do
  app <- HA.selectElement (QuerySelector "#app")
  parent <- HA.selectElement (QuerySelector "#root")
  for_ parent \p -> do
    for_ app \a -> liftEffect (removeChild (toNode a) (toNode p))
    runUI (component fixture) unit p
