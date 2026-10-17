-- | The typeahead scenario's fake server, the same for every app.
module Suite.Shared.Typeahead
  ( Fixture
  , search
  ) where

import Prelude

import Data.Array as Array
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)

-- | The scenario's fixture: the quiet time before a query is sent, the
-- | server's latency, both in ms, and what it searches.
type Fixture = { debounce :: Number, latency :: Number, fruits :: Array String }

-- | Up to eight fruits that contain the query, after the latency.
search :: Fixture -> String -> Aff (Array String)
search fixture query = do
  delay (Milliseconds fixture.latency)
  pure $ Array.take 8 $ Array.filter (String.contains (String.Pattern query)) fixture.fruits
