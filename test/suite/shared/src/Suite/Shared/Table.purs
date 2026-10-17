-- | The table scenario's rows, the same for every app: js-framework-benchmark's
-- | labels, picked by a seeded pseudo-random sequence so that every app
-- | builds the same rows from the same seed.
module Suite.Shared.Table
  ( TableRow
  , Source
  , source
  , rows
  , swap
  , updateEvery
  ) where

import Prelude

import Data.Array as Array
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number ((%))
import Data.Traversable (mapAccumL)

type TableRow = { id :: Int, label :: String }

-- | Where the next rows come from: the generator's state and the next id.
type Source = { seed :: Number, next :: Int }

source :: Number -> Source
source seed = { seed, next: 1 }

-- | `n` new rows, and the source after them.
rows :: Int -> Source -> { rows :: Array TableRow, source :: Source }
rows n s =
  let
    made = mapAccumL row s.seed (Array.range s.next (s.next + n - 1))
  in
    { rows: if n <= 0 then [] else made.value, source: { seed: made.accum, next: s.next + max 0 n } }
  where
  row seed id =
    let
      a = step seed
      b = step a
      c = step b
    in
      { accum: c, value: { id, label: pick adjectives a <> " " <> pick colors b <> " " <> pick nouns c } }

-- | Park and Miller's minimal standard generator, exact in a `Number`.
step :: Number -> Number
step seed = (seed * 48271.0) % 2147483647.0

pick :: Array String -> Number -> String
pick words seed = fromMaybe "" (Array.index words (floor (seed / 2147483647.0 * toNumber (Array.length words))))

-- | The benchmark's swap: the second row and the 999th.
swap :: forall a. Array a -> Array a
swap xs = case Array.index xs 1, Array.index xs 998 of
  Just a, Just b -> fromMaybe xs (Array.updateAt 1 b xs >>= Array.updateAt 998 a)
  _, _ -> xs

-- | The benchmark's update: " !!!" added to every 10th row's label, from
-- | the first.
updateEvery :: forall r. Array { label :: String | r } -> Array { label :: String | r }
updateEvery = Array.mapWithIndex \i row -> if i `mod` 10 == 0 then row { label = row.label <> " !!!" } else row

adjectives :: Array String
adjectives =
  [ "pretty", "large", "big", "small", "tall", "short", "long", "handsome", "plain", "quaint", "clean"
  , "elegant", "easy", "angry", "crazy", "helpful", "mushy", "odd", "unsightly", "adorable", "important"
  , "inexpensive", "cheap", "expensive", "fancy"
  ]

colors :: Array String
colors = [ "red", "yellow", "blue", "green", "pink", "brown", "purple", "brown", "white", "black", "orange" ]

nouns :: Array String
nouns =
  [ "table", "chair", "house", "bbq", "desk", "car", "pony", "cookie", "sandwich", "burger", "pizza", "mouse"
  , "keyboard"
  ]
