-- | Drives a scenario's app in Chromium through Playwright, and bundles the
-- | apps, every one with the same esbuild settings.
module Suite.Driver
  ( Browser
  , Page
  , bundle
  , launch
  , close
  , open
  , closePage
  , pageErrors
  , click
  , fill
  , settle
  , snapshot
  , now
  , gzipSize
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign)
import Suite.Dom (Node(..))
import Suite.Scenario (TestId)

foreign import data Browser :: Type
foreign import data Page :: Type

foreign import bundleImpl :: String -> String -> EffectFnAff Unit
foreign import launchImpl :: String -> EffectFnAff Browser
foreign import closeImpl :: Browser -> EffectFnAff Unit
foreign import openImpl :: Browser -> String -> EffectFnAff Page
foreign import closePageImpl :: Page -> EffectFnAff Unit
foreign import errors :: Page -> Effect (Array String)
foreign import clickImpl :: Page -> TestId -> EffectFnAff Unit
foreign import fillImpl :: Page -> TestId -> String -> EffectFnAff Unit
foreign import settleImpl :: Page -> EffectFnAff Unit
foreign import snapshotImpl :: Page -> EffectFnAff Foreign
foreign import toNodes
  :: (String -> Array (Tuple String String) -> Array Node -> Node)
  -> (String -> Node)
  -> (String -> String -> Tuple String String)
  -> Foreign
  -> Array Node

-- | Bundles the compiled module's `main`, called with `window.psxFixture`,
-- | into one minified script.
bundle :: { module :: String, outfile :: String } -> Aff Unit
bundle { module: m, outfile } = fromEffectFnAff (bundleImpl m outfile)

-- | Chromium at the given path, or Playwright's own when the path is empty.
launch :: String -> Aff Browser
launch = fromEffectFnAff <<< launchImpl

close :: Browser -> Aff Unit
close = fromEffectFnAff <<< closeImpl

-- | Opens an HTML file in a new page.
open :: Browser -> String -> Aff Page
open browser file = fromEffectFnAff (openImpl browser file)

closePage :: Page -> Aff Unit
closePage = fromEffectFnAff <<< closePageImpl

-- | Uncaught exceptions and console errors so far.
pageErrors :: Page -> Effect (Array String)
pageErrors = errors

click :: Page -> TestId -> Aff Unit
click page testId = fromEffectFnAff (clickImpl page testId)

fill :: Page -> TestId -> String -> Aff Unit
fill page testId text = fromEffectFnAff (fillImpl page testId text)

-- | Waits until the framework has rendered what the last step caused.
settle :: Page -> Aff Unit
settle = fromEffectFnAff <<< settleImpl

-- | The DOM under the page's `#root`, before normalising.
snapshot :: Page -> Aff (Array Node)
snapshot page = toNodes Element Text Tuple <$> fromEffectFnAff (snapshotImpl page)

-- | Milliseconds, for timing.
foreign import now :: Effect Number

-- | The file's size in bytes after gzip at its highest level.
foreign import gzipSize :: String -> Effect Int
