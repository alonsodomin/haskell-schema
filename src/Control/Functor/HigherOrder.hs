{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}

module Control.Functor.HigherOrder where

import Control.Natural

type HAlgebra f g = f g :~> g

class HFunctor f where
  hfmap :: (m :~> n) -> f m :~> f n
  ffmap :: Functor g => (a -> b) -> f g a -> f g b

class HFunctor f => HPointed f where
  hreturn :: Functor g => g a -> f g a

class HFunctor f => HCopointed f where
  hextract :: Functor g => f g a -> g a

newtype HFix f a = HFix { unfix :: f (HFix f) a }

data HCofree a f i = HCofree { hask :: a, hnext :: f (HCofree a f) i }

-- hforget :: HFunctor f => HCofree a f :~> HFix f
-- hforget = wrapNT $ (\c -> hfmap (hnext c))

--data HCofree a f i = HCofree { ask :: a,  }