-- | For a framework that mounts by appending: the pages hold `#app` inside
-- | `#root`, and the contracts are what `#root` holds, so an app that
-- | appends removes `#app` and mounts on `#root`.
module Suite.Shared.Dom (removeApp) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Web.DOM.ChildNode (remove)
import Web.DOM.Element (toChildNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

removeApp :: Effect Unit
removeApp = do
  doc <- window >>= document
  app <- querySelector (QuerySelector "#app") (toParentNode doc)
  for_ app (remove <<< toChildNode)
