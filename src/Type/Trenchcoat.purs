module Type.Trenchcoat
  ( Trenchcoat
  , class PseudoFunctor
  , pseudoMap
  , disguise
  , undisguise
  , undercover
  ) where

import Prelude

-- okay so I just now realized it'll probably take some like super wacky
-- row magic to make a single Trenchcoat value capable of adapting multiple classes
-- (though now that I'm thinking about it that specific way, the metadata for it could
-- literally just. be stored inside a record. and use another class to map 
-- ((Type -> Type) -> Constraint)s to Symbols I guess idk)
-- but anyways I'm just going to start with functors as a proof of concept before I get
-- lost in the weeds on that lmao. and just for the String-like case, then think about
-- how to do constrained actual HKTs like Set later

data Trenchcoat :: Type -> Type -> Type -> Type
data Trenchcoat f a b = Trenchcoat f (a -> b)

class PseudoFunctor :: Type -> Type -> Constraint
class PseudoFunctor f a where
  pseudoMap :: (a -> a) -> f -> f

disguise :: forall f a. PseudoFunctor f a => f -> Trenchcoat f a a
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
