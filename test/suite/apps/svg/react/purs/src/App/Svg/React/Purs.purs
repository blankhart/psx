-- | The svg scenario on React, hand-written against React's own API,
-- | with its SVG elements from `React.Svg`: the control for
-- | `App.Svg.React.Psx`.
module App.Svg.React.Purs (main) where

import Prelude

import Suite.React.Native as P
import Suite.React.Native as S

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Suite.React as React

type Fixture = { values :: Array Int, max :: Int }

type Model = { values :: Array Int, max :: Int, selected :: Maybe Int }

data Msg = Select Int | Grow

update :: Model -> Msg -> Model
update model = case _ of
  Select i -> model { selected = Just i }
  Grow -> case model.selected of
    Just i -> model { values = fromMaybe model.values (Array.modifyAt i (\v -> min model.max (v + 1)) model.values) }
    Nothing -> model

-- | A bar's left edge and barTop, in the chart's coordinates.
left :: Int -> Int
left i = 10 + 20 * i

barTop :: Int -> Int
barTop v = 55 - 4 * v

-- | The line through the tops of the bars.
trend :: Array Int -> String
trend values = String.joinWith " " (Array.mapWithIndex point values)
  where
  point i v = (if i == 0 then "M " else "L ") <> show (left i + 8) <> " " <> show (barTop v)

view :: (Msg -> Effect Unit) -> Model -> P.Html
view dispatch model =
  P.figure [ P.class' "chart", testId "chart" ]
    [ S.svg [ S.viewBox "0 0 120 60", S.width "360", S.height "180" ]
        ( [ S.g [ S.class' "bars" ]
              ( Array.mapWithIndex
                  ( \i v ->
                      S.rect
                        [ P.key (show i)
                        , testId ("bar-" <> show i)
                        , S.x (show (left i))
                        , S.y (show (barTop v))
                        , S.width "16"
                        , S.height (show (4 * v))
                        , S.fill (if model.selected == Just i then "orange" else "steelblue")
                        , P.onClick (dispatch ((Select i)))
                        ]
                        []
                  )
                  model.values
              )
          , S.path [ S.class' "trend", S.d (trend model.values), S.fill "none", S.stroke "gray" ] []
          , S.line [ S.x1 "5", S.y1 "55", S.x2 "115", S.y2 "55", S.stroke "black" ] []
          ]
            <> case model.selected of
              Just i -> [ S.circle [ S.cx (show (left i + 8)), S.cy (show (barTop (value i))), S.r "2", S.fill "orange" ] [] ]
              Nothing -> []
            <> [ S.svgText [ S.x "5", S.y "8" ] [ P.text "Values" ] ]
        )
    , P.figcaption [ testId "caption" ]
        [ P.text case model.selected of
            Just i -> "Bar " <> show i <> ": " <> show (value i)
            Nothing -> "Click a bar"
        ]
    , P.button [ testId "grow", P.onClick (dispatch (Grow)), P.disabled (model.selected == Nothing) ] [ P.text "Grow" ]
    ]
  where
  value i = fromMaybe 0 (Array.index model.values i)

testId :: String -> P.Trait
testId = P.attribute "data-testid"

main :: Fixture -> Effect Unit
main fixture = React.mount ({ values: fixture.values, max: fixture.max, selected: Nothing }) update (\dispatch model -> view dispatch model)
