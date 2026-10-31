module Suite.React (mount, mountWith) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic (JSX)
import React.Basic.Hooks as React
import React.Basic.DOM.Client as Client
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)

mount :: forall model msg. model -> (model -> msg -> model) -> ((msg -> Effect Unit) -> model -> JSX) -> Effect Unit
mount initial update = mountWith initial (\set _ msg -> set (\current -> update current msg))

mountWith :: forall model msg. model -> (((model -> model) -> Effect Unit) -> model -> msg -> Effect Unit) -> ((msg -> Effect Unit) -> model -> JSX) -> Effect Unit
mountWith initial update view = do
  component <- React.component "PSX suite" \_ -> React.do
    Tuple model set <- React.useState initial
    pure (view (update set model) model)
  parent <- window >>= document >>= querySelector (QuerySelector "#root") <<< toParentNode
  case parent of
    Nothing -> throw "Missing #root"
    Just element -> do
      root <- Client.createRoot element
      Client.renderRoot root (component unit)
