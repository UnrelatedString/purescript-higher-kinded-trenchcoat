module Type.Trenchcoat
  ( Trenchcoat
  , Hollow(..)
  , class Contains
  , endoMap
  , class PseudoFunctor
  , pseudoMap
  , NothingToSeeHere
  , disguise
  , undisguise
  , undercover
  ) where

import Prelude
import Data.Set as Set
import Data.String.CodePoints (CodePoint, toCodePointArray, fromCodePointArray)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.Functor.Variant (VariantF, inj, on, case_)
import Prim.RowList as RL
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- | Lifts an arbitrary type constructor to a correct-by-construction `Functor`.
-- | May be renamed or restructured in a future release.
newtype Trenchcoat :: (Type -> Type) -> Row (Type -> Type) -> Type -> Type
newtype Trenchcoat f r b = Trenchcoat (VariantF r b)

derive newtype instance Functor (Trenchcoat f r)

data Trenchcoat' :: (Type -> Type) -> Type -> Type -> Type
data Trenchcoat' f a b = Trenchcoat' (f a) (a -> b)

instance Functor (Trenchcoat' f a) where
  -- | Dumb composition: the backbone of `undisguise`.
  map f (Trenchcoat' v g) = Trenchcoat' v $ f <<< g

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

instance Contains v a => Contains (Hollow v b) a where
  endoMap = coerce (endoMap :: (a -> a) -> v -> v)

instance Contains v a => PseudoFunctor (Hollow v) a a where
  pseudoMap = coerce (endoMap :: (a -> a) -> v -> v)

-- | Placeholder label for automatically-inferred `Variant`s.
type NothingToSeeHere :: Symbol
type NothingToSeeHere = "nothing"

-- | Wrap something in a `Trenchcoat`.
disguise :: forall f r a. f a -> Trenchcoat f (nothing :: Trenchcoat' f a | r) a
disguise v = Trenchcoat $ inj @NothingToSeeHere $ Trenchcoat' v identity

-- | Take a `PseudoFunctor`'s `Trenchcoat` off.
undisguise
  :: forall f r rl b
  . RL.RowToList r rl
  => AllUndisguise rl f b
  => Trenchcoat f r b -> f b
undisguise (Trenchcoat var) = undisguiseInner

undisguise' :: forall f a b. PseudoFunctor f a b => Trenchcoat' f a b -> b
undisguise' (Trenchcoat' v f) = pseudoMap f v

-- | Evaluate a function on a `disguise`d `PseudoFunctor`.
undercover
  :: forall f a b
  . PseudoFunctor f a b
  => (Trenchcoat (f a) a a -> Trenchcoat (f a) a b)
  -> f a
  -> f b
undercover g = undisguise <<< g <<< disguise

class AllUndisguise :: RL.RowList (Type -> Type) -> (Type -> Type) -> Type -> Constraint
class AllUndisguise rl f b where
  undisguiseInner :: forall r. RL.RowToList r rl => VariantF r b -> b

instance AllUndisguise RL.Nil f b where
  undisguiseInner = unsafeCoerce case_

else
instance
  ( PseudoFunctor f a b
  , AllUndisguise tail f b
  )
  => AllUndisguise (RL.Cons sym (Trenchcoat' f a) tail) f b where
  undisguiseInner = on @sym undisguise' $ undisguiseInner

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
