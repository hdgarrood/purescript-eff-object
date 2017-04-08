module Test.Main where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Symbol (SProxy(..))
import EffObject (EffObject, ReadOnly, ReadWrite, readProperty, writeProperty,
                  BoundMethod, bindProperty)

type ExampleObject e =
  EffObject
    (console :: CONSOLE | e)
    ( foo :: ReadOnly Int
    , bar :: ReadWrite String
    , baz :: ReadOnly (BoundMethod (EffFn1 (console :: CONSOLE | e) Int Int))
    )

foreign import mkExample :: forall e1 e2. Eff e1 (ExampleObject e2)

main :: Eff (console :: CONSOLE) Unit
main = do
  eg <- mkExample
  readProperty (SProxy :: SProxy "foo") eg >>= logShow
  readProperty (SProxy :: SProxy "bar") eg >>= log
  writeProperty (SProxy :: SProxy "bar") eg "hello"
  readProperty (SProxy :: SProxy "bar") eg >>= log

  baz <- bindProperty (SProxy :: SProxy "baz") eg
  runEffFn1 baz 4 >>= logShow

  -- Uncommenting this will cause a custom type error: "Cannot write to
  -- a read-only property."
  -- writeProperty (SProxy :: SProxy "foo") eg 3
