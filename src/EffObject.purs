module EffObject
  ( EffObject
  , readProperty
  , writeProperty
  , BoundMethod
  , mkBoundMethod
  , bind
  ) where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)

-- | A JavaScript object, with properties which can be read or written. The
-- | type arguments track:
-- |
-- | * `e`: effects associated with reading or writing attributes
-- | * `r`: readable properties
-- | * `w`: writable properties
data EffObject
  (e :: # Effect)
  (r :: # Type)
  (w :: # Type)

-- | Read a property from an `EffObject`.
readProperty ::
  forall e r r' w name a.
  IsSymbol name =>
  RowCons name a r' r =>
  SProxy name ->
  EffObject e r w ->
  Eff e a
readProperty prx obj =
  unsafeReadProperty (reflectSymbol prx) obj

foreign import unsafeReadProperty ::
  forall obj e a. String -> obj -> Eff e a

-- | Write a property to an `EffObject`.
writeProperty ::
  forall e r w w' name a.
  IsSymbol name =>
  RowCons name a w' w =>
  SProxy name ->
  EffObject e r w ->
  a ->
  Eff e Unit
writeProperty prx obj val =
  unsafeWriteProperty (reflectSymbol prx) obj val

foreign import unsafeWriteProperty ::
  forall obj e a. String -> obj -> a -> Eff e Unit

-- | A method which should be called with a `this` value which is a specific
-- | kind of `EffObject`. The `fn` type argument should generally either be an
-- | `EffFn` from `purescript-eff`, or a `Fn` from `purescript-functions`.
newtype BoundMethod e r w fn = BoundMethod (EffObject e r w -> fn)

-- | Wrap a function in a `BoundMethod` constructor to ensure that it may only
-- | be called with a `this` value of the type `EffObject e r w`.
mkBoundMethod :: forall fn e r w. fn -> BoundMethod e r w fn
mkBoundMethod fn = BoundMethod (unsafeBind fn)

-- | Run a `BoundMethod` by providing a `this` object.
bind :: forall fn e r w. EffObject e r w -> BoundMethod e r w fn -> fn
bind obj (BoundMethod f) = f obj

foreign import unsafeBind :: forall fn e r w. fn -> EffObject e r w -> fn
