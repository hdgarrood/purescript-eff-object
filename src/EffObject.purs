-- | This module contains declarations and some basic functions to allow you to
-- | give PureScript types to stateful or otherwise effectful JavaScript APIs
-- | like XMLHttpRequest or WebSocket without writing a ton of error-prone and
-- | tedious FFI code.
-- |
-- | For example, if you wanted to provide a low-level typing for the
-- | XMLHttpRequest web API, you might write the following:
-- |
-- | ```purescript
-- | type XMLHttpRequest e =
-- |   EffObject
-- |     -- The first type argument contains effects associated with interacting
-- |     -- with the object. Interacting with an XHR uses an AJAX effect, so
-- |     -- we record this here.
-- |     ( ajax :: AJAX )
-- |     -- We record the properties as well as their types and access levels
-- |     -- in the second type argument.
-- |     ( onreadystatechange :: ReadWrite (Eff e Unit)
-- |     , readyState :: ReadOnly Int
-- |     , response :: ReadOnly (Nullable Response)
-- |     , [...]
-- |     )
-- | ```
-- |
-- | Consumers of your API can now interact with an `XMLHttpRequest` object
-- | using the `readProperty` or `writeProperty` functions:
-- |
-- | ```purescript
-- | checkReadyState ::
-- |   forall e.
-- |   XMLHttpRequest e ->
-- |   Eff (ajax :: AJAX | e) String
-- | checkReadyState req = do
-- |   s <- readProperty (SProxy :: SProxy "readyState") req
-- |   pure $ if (s == done)
-- |     then "Done!"
-- |     else "Not done."
-- |   where
-- |     done = 4
-- | ```
-- |
-- | Attempting to write read-only properties will produce a custom type error
-- | explaining what happened:
-- |
-- | ```purescript
-- | oops :: forall e. XMLHttpRequest e -> Eff (ajax :: AJAX | e) Unit
-- | oops req = writeProperty (SProxy :: SProxy "readyState") req 0
-- | -- Throws:
-- | --   A custom type error occurred while solving type class constraints:
-- | --     Cannot write to a read-only property.
-- | ```
-- |
-- | Note that any APIs constructed using this library will necessarily be
-- | very low-level; you might want to build extra layers above this API, for
-- | instance, to provide a real `ReadyState` type which is guaranteed to only
-- | take values which would be valid the for `readyState` property itself.
module EffObject
  ( EffObject
  , class Readable
  , class Writeable
  , ReadOnly
  , WriteOnly
  , ReadWrite
  , readProperty
  , writeProperty
  , BoundFunction
  , BoundMethod
  , Self
  , mkBoundFunction
  , bindTo
  , bindProperty
  ) where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)

data ReadOnly (a :: Type)
data WriteOnly (a :: Type)
data ReadWrite (a :: Type)

class Readable (a :: Type -> Type)

instance readableReadOnly :: Readable ReadOnly
instance readableReadWrite :: Readable ReadWrite

instance notReadableWriteOnly ::
  Fail "Cannot read from a write-only property." => Readable WriteOnly

class Writeable (a :: Type -> Type)

instance writeableWriteOnly :: Writeable WriteOnly
instance writeableReadWrite :: Writeable ReadWrite

instance notWriteableReadOnly ::
  Fail "Cannot write to a read-only property." => Writeable ReadOnly

-- | A JavaScript object, with properties which can be read or written. The
-- | type arguments track:
-- |
-- | * `e`: Effects associated with reading or writing properties
-- | * `props`: Properties.
-- |
-- | The labels in the `props` row type must correspond exactly to the property
-- | names on the underlying object, and the types in the `props` row should
-- | be of the form `ReadOnly a`, `WriteOnly a`, or `ReadWrite a`, depending
-- | on the access level, and where `a` is the type of the property's value.
data EffObject
  (e :: # Effect)
  (props :: # Type)

-- | Read a property from an `EffObject`.
readProperty ::
  forall e prop props props' name access a.
  IsSymbol name =>
  Readable access =>
  RowCons name prop props' props =>
  TypeEquals prop (access a) =>
  SProxy name ->
  EffObject e props ->
  Eff e a
readProperty prx obj =
  unsafeReadProperty (reflectSymbol prx) obj

foreign import unsafeReadProperty ::
  forall obj e a. String -> obj -> Eff e a

-- | Write a property to an `EffObject`.
writeProperty ::
  forall e prop props props' name access a.
  IsSymbol name =>
  Writeable access =>
  RowCons name prop props' props =>
  TypeEquals prop (access a) =>
  SProxy name ->
  EffObject e props ->
  a ->
  Eff e Unit
writeProperty prx obj val =
  unsafeWriteProperty (reflectSymbol prx) obj val

foreign import unsafeWriteProperty ::
  forall obj e a. String -> obj -> a -> Eff e Unit

-- | A function which should be called with a `this` value which is a specific
-- | type. The `fn` type argument should generally either be an
-- | `EffFn` from `purescript-eff`, or a `Fn` from `purescript-functions`.
newtype BoundFunction (receiver :: Type) (fn :: Type)
  = BoundFunction fn

-- | Wrap a function in a `BoundFunction` constructor to ensure that it may only
-- | be called with a `this` value of the type `receiver`.
mkBoundFunction :: forall fn receiver. fn -> BoundFunction receiver fn
mkBoundFunction = BoundFunction

-- | Run a `BoundFunction` by providing a `this` object.
bindTo :: forall fn receiver. receiver -> BoundFunction receiver fn -> fn
bindTo obj (BoundFunction f) = unsafeBindTo f obj

foreign import unsafeBindTo :: forall fn receiver. fn -> receiver -> fn

-- | This type provides a way of saying that a `BoundFunction` which is a
-- | property of some `EffObject` should receive that `EffObject` as its `this`
-- | value when called.
data Self

toSelf :: forall a. a -> Self
toSelf = unsafeCoerce

type BoundMethod fn
  = BoundFunction Self fn

bindProperty ::
  forall e prop props props' name access fn.
  IsSymbol name =>
  Readable access =>
  RowCons name prop props' props =>
  TypeEquals prop (access (BoundMethod fn)) =>
  SProxy name ->
  EffObject e props ->
  Eff e fn
bindProperty prx obj =
  map (bindTo (toSelf obj)) (readProperty prx obj)
