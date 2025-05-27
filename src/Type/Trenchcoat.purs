module Type.Trenchcoat
  ( Trenchcoat
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
import Type.Prelude (class TypeEquals, type ($))

-- | May be renamed or restructured in a future release.
data Trenchcoat :: Type -> Type -> Type -> Type
data Trenchcoat f a b = Trenchcoat f (a -> b)

-- | A class for type constructors which permit a `map`-like operation
-- | subject to a constraint on the input and output types.
-- | `pseudoMap` may freely violate the functor laws,
-- | but `Functor Trenchcoat f a` is correct by construction.
class PseudoFunctor :: (Type -> Type) -> (Type -> Constraint) -> Constraint
class PseudoFunctor f c where
  pseudoMap :: forall a b. (c$a) => (c$b) => (a -> b) -> f a -> f b

-- | An alias for `PseudoFunctor`-like concrete types.
type PseudoFunctor0 :: Type -> Type -> Constraint
type PseudoFunctor0 f a = PseudoFunctor (Const f) (TypeEquals a)

instance PseudoFunctor f c => (c$a) => Functor (Trenchcoat f a) where
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
