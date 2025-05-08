module Type.Trenchcoat
  ( Trenchcoat
  , class PseudoFunctor
  , pseudoMap
  , disguise
  , undisguise
  , undercover
  ) where

import Prelude

import Data.Set as Set
import Data.String.CodePoints (CodePoint, toCodePointArray, fromCodePointArray)

-- okay so I just now realized it'll probably take some like super wacky
-- row magic to make a single Trenchcoat value capable of adapting multiple classes
-- (though now that I'm thinking about it that specific way, the metadata for it could
-- literally just. be stored inside a record. and use another class to map 
-- ((Type -> Type) -> Constraint)s to Symbols I guess idk)
-- but anyways I'm just going to start with functors as a proof of concept before I get
-- lost in the weeds on that lmao. and just for the String-like case, then think about
-- how to do constrained actual HKTs like Set later

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

data Trenchcoat :: Type -> Type -> Type -> Type
data Trenchcoat f a b = Trenchcoat f (a -> b)

class PseudoFunctor :: Type -> Type -> Constraint
class PseudoFunctor f a where
  pseudoMap :: (a -> a) -> f -> f

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

-- | this is kinda stupid because it's literally less flexible than
-- | the normal Data.Set.map but at least it's not as pointless as the String one
instance Ord a => PseudoFunctor (Set.Set a) a where
  pseudoMap = Set.map
