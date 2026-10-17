-- | A search box over a slow fake server, after Screamer's Typeahead
-- | example: the query is sent once typing pauses for `debounce` ms, the
-- | server answers after `latency` ms with up to eight fruits that contain
-- | the query, an answer to an older query is never shown, the arrow keys
-- | move the selection and Enter chooses. Queries shorter than three
-- | letters are not sent.
-- |
-- | The status reads "Type at least three letters" for a short query,
-- | "Searching…" until the answer to the current query arrives, then the
-- | number of results. The contracts after a search are `Await`ed, so only
-- | the settled states are part of them.
module Suite.Scenario.Typeahead (typeahead) where

import Suite.Scenario (Scenario, Step(..))

typeahead :: Scenario
typeahead =
  { name: "typeahead"
  , fixture:
      """{ "debounce": 100
         , "latency": 50
         , "fruits":
           [ "apple", "apricot", "banana", "bilberry", "blackberry", "blueberry"
           , "boysenberry", "cherry", "cloudberry", "cranberry", "elderberry", "gooseberry"
           , "grape", "huckleberry", "lemon", "lime", "mango", "mangosteen", "mulberry"
           , "orange", "papaya", "peach", "pear", "plum", "raspberry", "strawberry"
           ]
         }"""
  , steps:
      [ Expect "initial"
      , Type "search" "be"
      , Expect "short"
      , Type "search" "berry"
      , Await "berries"
      , Press "search" "ArrowDown"
      , Press "search" "ArrowDown"
      , Expect "third"
      , Press "search" "ArrowUp"
      , Expect "second"
      , Press "search" "Enter"
      , Expect "chosen"
      , Type "search" "man"
      , Await "mangoes"
      , Type "search" "ma"
      , Await "cleared"
      , Type "search" "pea"
      , Await "peas"
      , Measure
          { name: "move the selection"
          , runs: 5
          , setup: []
          , steps: [ Press "search" "ArrowDown", Press "search" "ArrowUp" ]
          }
      , Expect "peas"
      ]
  }
