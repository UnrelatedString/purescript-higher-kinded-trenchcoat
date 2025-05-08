module Type.Trenchcoat
  ( Trenchcoat
  , class PseudoFunctor
  --, class PartialFunctor
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
import Type.Prelude (class TypeEquals)

-- okay so I just now realized it'll probably take some like super wacky
-- row magic to make a single Trenchcoat value capable of adapting multiple classes
-- (though now that I'm thinking about it that specific way, the metadata for it could
-- literally just. be stored inside a record. and use another class to map 
-- ((Type -> Type) -> Constraint)s to Symbols I guess idk)
-- but anyways I'm just going to start with functors as a proof of concept before I get
-- lost in the weeds on that lmao.
-- and turns out the constraint thing actually is pretty easy to roll in 
-- ...... except do I really want to fuse it like this or do I want to think of a more
-- generic way to reuse the machinery and ughhhhhhhhhhhhhhhhhhhhh

-- also really torn about how or if to do stuff like Apply where it's like,
-- a Set literally can't contain functions but I can still see it being useful to
-- pretend it can for stuff like (+) <$> set1 <*> set2
-- so what the hell do I make PseudoApply's member to supply the structural way
-- to do that? like, at that point does it actually just have to round trip in and out
-- of another type? that's kinda pointless which makes me sad ... ;_;

-- or wait for like. for Foldable. since like that fundamentally doesn't have undisguise
-- ...or for Unfoldable since that fundamentally doesn't have disguise
-- ........
-- yeah good thing I started small jgf;aojg ;erg ;
-- and what even other classes are there to support? Bifunctor?? maybe
-- but anyways I'm starting to think it might have to like nest the structure so
-- undisguise doesn't have to magically know every way it has to compose multiple
-- like layers of metadata or whatever...?

data Trenchcoat :: Type -> Type -> Type -> Type
data Trenchcoat f a b = Trenchcoat f (a -> b)

class PseudoFunctor :: Type -> Type -> Constraint
class PseudoFunctor f a where
  pseudoMap :: (a -> a) -> f -> f

instance PseudoFunctor f a => Functor (Trenchcoat f a) where
  map f (Trenchcoat v g) = Trenchcoat v $ f <<< g

disguise :: forall f a. f -> Trenchcoat f a a
disguise = flip Trenchcoat identity

undisguise :: forall f a. PseudoFunctor f a => Trenchcoat f a a -> f
undisguise (Trenchcoat f g) = pseudoMap g f

undercover
  :: forall f a
  . PseudoFunctor f a
  => (Trenchcoat f a a -> Trenchcoat f a a)
  -> f
  -> f
undercover g = undisguise <<< g <<< disguise

-- | this literally just round trips to and from Array so it's kinda super pointless
-- | but uhhhhh yeah proof of concept ehehehehe ahahahahahaha lol. :/
instance PseudoFunctor String CodePoint where
  pseudoMap f = fromCodePointArray <<< map f <<< toCodePointArray

instance PseudoFunctor String Char where
  pseudoMap f = fromCharArray <<< map f <<< toCharArray

-- | this is kinda stupid because it's literally less flexible than
-- | the normal Data.Set.map but at least it's not as pointless as the String one

-- Also for when this is more flexible note that composing
-- undercover maps will not always be consistent with a single
-- composed undercover map if the type in between has a weird Eq instance
-- that doesn't obey the non-existent law `a == a = f a == f a`
instance Ord a => PseudoFunctor (Set.Set a) a where
  pseudoMap = Set.map
