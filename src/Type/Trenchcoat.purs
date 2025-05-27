module Type.Trenchcoat
  ( Trenchcoat
  , Hollow(..)
  , class Contains
  , endoMap
  , class PseudoFunctor
  , pseudoMap
  , disguise
  , undisguise
  , undercover
  ) where

import Prelude

import Data.Set as Set
import Data.String.CodePoints (CodePoint, toCodePointArray, fromCodePointArray)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Safe.Coerce (coerce)

-- | Lifts an arbitrary concrete type to a correct-by-construction `Functor`.
-- | May be renamed or restructured in a future release.
data Trenchcoat :: Type -> Type -> Type -> Type
data Trenchcoat v a b = Trenchcoat v (a -> b)

-- | Equivalent to `Const`, but without its `Functor` instance,
-- | purely because it would be weird for that to be inconsistent with
-- | its `PseudoFunctor` instance.
-- | Also restricted to concrete ignored types.
newtype Hollow :: Type -> Type -> Type
newtype Hollow a b = Hollow a

-- | A class for concrete types which support `Functor`-like operations on
-- | a constant "item" type.
-- | The item type is not considered a functional dependency,
-- | as one type may sensibly admit multiple interpretations of its contents.
class Contains v a where
  endoMap :: (a -> a) -> v -> v

-- | A class for type constructors which permit a `map`-like operation
-- | subject to arbitrary constraints on the input and output types.
-- | `pseudoMap` may freely violate the functor laws
-- | if considered in the place of `map`,
-- | but `Functor (Trenchcoat f a)` is correct by construction.
class PseudoFunctor :: (Type -> Type) -> Type -> Type -> Constraint
class PseudoFunctor f a b where
  pseudoMap :: (a -> b) -> f a -> f b

instance PseudoFunctor f a b => Functor (Trenchcoat (f a) a) where
  map f (Trenchcoat v g) = Trenchcoat v $ f <<< g

instance Contains v a => Contains (Hollow v a) a where
  endoMap = pseudoMap

instance Contains v a => PseudoFunctor (Hollow v) a a where
  pseudoMap = coerce (endoMap :: (a -> a) -> v -> v)

-- | Wrap something in a `Trenchcoat`.
disguise :: forall v a. v -> Trenchcoat v a a
disguise = flip Trenchcoat identity

-- | Take a `PseudoFunctor`'s `Trenchcoat` off.
undisguise :: forall f a b. PseudoFunctor f a b => Trenchcoat (f a) a b -> f b
undisguise (Trenchcoat f g) = pseudoMap g f

-- | Evaluate a function on a `disguise`d `PseudoFunctor`.
undercover
  :: forall f a b
  . PseudoFunctor f a b
  => (Trenchcoat (f a) a a -> Trenchcoat (f a) a b)
  -> f a
  -> f b
undercover g = undisguise <<< g <<< disguise

-- | `endoMap` is implemented as a round trip conversion through an `Array`.
-- | For practical use, it may be more convenient to simply use this `Array` yourself.
instance Contains String CodePoint where
  endoMap f = fromCodePointArray <<< map f <<< toCodePointArray

-- | `endoMap` is implemented as a round trip conversion through an `Array`.
-- | For practical use, it may be more convenient to simply use this `Array` yourself.
instance Contains String Char where
  endoMap f = fromCharArray <<< map f <<< toCharArray

instance (Ord a, Ord b) => PseudoFunctor Set.Set a b where
  pseudoMap = Set.map
