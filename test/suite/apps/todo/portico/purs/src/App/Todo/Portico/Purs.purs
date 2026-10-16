-- | The todo scenario on Portico, hand-written against Portico's own API: the
-- | control for `App.Todo.Portico.Psx`.
module App.Todo.Portico.Purs (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Effect (Effect)
import Portico ((:>))
import Portico as P

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

view :: Model -> P.Html Msg
view model =
  P.section [ P.class' "todo", testId "todo" ]
    [ P.h1 [] [ P.text "Todos" ]
    , P.div [ P.class' "new" ]
        [ P.input [ P.type' "text", testId "draft", P.value model.draft, P.onInput Draft ] []
        , P.button [ testId "add", P.onClick Add, P.disabled (model.draft == "") ] [ P.text "Add" ]
        ]
    , P.ul [ P.class' "items" ] (map item (Array.filter (shown model.filter) model.todos))
    , P.p [ testId "left" ] [ P.text (show (Array.length (Array.filter (not <<< _.done) model.todos)) <> " left") ]
    , P.div [ P.class' "filters" ]
        [ P.button [ testId "all", P.onClick (Show All), P.disabled (model.filter == All) ] [ P.text "All" ]
        , P.button [ testId "active", P.onClick (Show Active), P.disabled (model.filter == Active) ] [ P.text "Active" ]
        , P.button [ testId "completed", P.onClick (Show Completed), P.disabled (model.filter == Completed) ] [ P.text "Completed" ]
        ]
    ]
  where
  item todo =
    P.li [ P.key (show todo.id), P.class' (if todo.done then "item done" else "item") ]
      [ P.input [ P.type' "checkbox", testId ("toggle-" <> show todo.id), P.checked todo.done, P.onChange (Toggle todo.id) ] []
      , P.span [] [ P.text todo.text ]
      , P.button [ testId ("remove-" <> show todo.id), P.onClick (Remove todo.id) ] [ P.text "Remove" ]
      ]

testId :: forall msg. String -> P.Trait msg
testId = P.attribute "data-testid"

-- | Mounts on `#app`, starting from the scenario's fixture.
main :: Fixture -> Effect Unit
main fixture =
  void $ P.mount "#app"
    { init: init fixture :> []
    , update: \model msg -> update model msg :> []
    , subscriptions: P.noSubscriptions
    , view: view
    }
