module Type.Trenchcoat
  ( Trenchcoat
  , class 
  , class PseudoFunctor
  , PseudoFunctor0
  , pseudoMap
  , disguise
  , undisguise
  , undercover
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Newtype (un)
import Data.Set as Set
import Data.String.CodePoints (CodePoint, toCodePointArray, fromCodePointArray)
import Data.String.CodeUnits (toCharArray, fromCharArray)

-- | Lifts an arbitrary concrete type to a correct-by-construction `Functor`.
-- | May be renamed or restructured in a future release.
data Trenchcoat :: Type -> Type -> Type -> Type
data Trenchcoat v a b = Trenchcoat v (a -> b)

-- | A class for concrete types which support `Functor`-like operations on
-- | a constant "item" type.
-- | The item type is not considered a functional dependency,
-- | as one type may sensibly admit multiple interpretations of its contents.
class PseudoFunctor (Const v) a a <= Contains v a where
  endoMap :: (a -> a) -> v -> v

-- | A class for type constructors which permit a `map`-like operation
-- | subject to arbitrary constraints on the input and output types.
-- | `pseudoMap` may freely violate the functor laws if taken to be `map`,
-- | but `Functor (Trenchcoat f a)` is correct by construction.
class PseudoFunctor :: (Type -> Type) -> Type -> Type -> Constraint
class PseudoFunctor f a b where
  pseudoMap :: (a -> b) -> f a -> f b

-- | Default implementation of `pseudoMap` to satisfy the
-- | `PseudoFunctor (Const v) a a` superclass bound for `Contains v a`.
pseudoMapDefaultEndo :: 

instance PseudoFunctor f a b => Functor (Trenchcoat f a) where
  map f (Trenchcoat v g) = Trenchcoat v $ f <<< g

disguise :: forall f a. f -> Trenchcoat f a a
disguise = flip Trenchcoat identity

undisguise :: forall f a b c. PseudoFunctor f c => (c$a) => (c$b) => Trenchcoat f a b -> f b
undisguise (Trenchcoat f g) = pseudoMap g f

undercover
  :: forall f a b c
  . PseudoFunctor f c
  => (Trenchcoat f a a -> Trenchcoat f a b)
  -> f a
  -> f b
undercover g = undisguise <<< g <<< disguise

-- | `pseudoMap` is implemented as a round trip conversion through an `Array`.
-- | For practical use, it may be more convenient to simply use this `Array` yourself.
instance PseudoFunctor0 String CodePoint where
  pseudoMap f = fromCodePointArray <<< map f <<< toCodePointArray

-- | `pseudoMap` is implemented as a round trip conversion through an `Array`.
-- | For practical use, it may be more convenient to simply use this `Array` yourself.
instance PseudoFunctor0 String Char where
  pseudoMap f = fromCharArray <<< map f <<< toCharArray

instance Ord a => PseudoFunctor0 (Set.Set a) a where
  pseudoMap = Set.map
