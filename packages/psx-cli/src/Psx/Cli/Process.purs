module Psx.Cli.Process (Result, runSync) where

import Effect (Effect)
import Data.Function.Uncurried (Fn2, runFn2)

type Result = { status :: Int, stdout :: String, stderr :: String }

foreign import runSync_ :: Fn2 String (Array String) (Effect Result)

runSync :: String -> Array String -> Effect Result
runSync = runFn2 runSync_
