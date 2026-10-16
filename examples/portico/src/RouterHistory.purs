module Examples.RouterHistory where

import Prelude

import Effect (Effect)
import Examples.Router (app)
import Portico as P
import Portico.Router as Router

-- The Router example on the History API. This one has to be loaded over HTTP
-- from the root of a server that answers every path with this page.
main :: Effect Unit
main = void $ P.mount "main" $ app Router.History
