module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Symbol (SProxy(..))
import EffObject (EffObject, readProperty, writeProperty)

type ExampleObject e =
  EffObject
    (console :: CONSOLE | e)
    (foo :: Int, bar :: String)
    (bar :: String)

foreign import mkExample :: forall e1 e2. Eff e1 (ExampleObject e2)

main :: Eff (console :: CONSOLE) Unit
main = do
  eg <- mkExample
  readProperty (SProxy :: SProxy "foo") eg >>= logShow
  readProperty (SProxy :: SProxy "bar") eg >>= log
  writeProperty (SProxy :: SProxy "bar") "hello" eg
  readProperty (SProxy :: SProxy "bar") eg >>= log
