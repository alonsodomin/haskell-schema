{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}

module Control.Functor.HigherOrder where

import           Control.Natural

type HAlgebra f g = f g :~> g

class HFunctor f where
  hfmap :: (m :~> n) -> f m :~> f n

class HFunctor f => HPointed f where
  hreturn :: Functor g => g a -> f g a

class HFunctor f => HCopointed f where
  hextract :: Functor g => f g a -> g a

newtype HFix f a = HFix { unfix :: f (HFix f) a }

data HEnvT
  (e :: *)
  (f :: ((* -> *) -> * -> *))
  (g :: (* -> *))
  (i :: *) = HEnvT { hask :: e, hlocal :: f g i }

type HCofree
  (a :: *)
  (f :: ((* -> *) -> * -> *))
  (i :: *) = HFix (HEnvT a f) i

instance HFunctor f => HFunctor (HEnvT a f) where
  hfmap nt = wrapNT $ \fm -> HEnvT (hask fm) (unwrapNT (hfmap nt) (hlocal fm))

hcata :: HFunctor f => HAlgebra f g -> HFix f :~> g
hcata alg = wrapNT $ \hf -> (unwrapNT alg) ((unwrapNT $ hfmap (hcata alg)) (unfix hf))

hforgetAlg :: HAlgebra (HEnvT a f) (HFix f)
hforgetAlg = wrapNT $ \env -> HFix $ hlocal env

hforget :: HFunctor f => HCofree a f i -> HFix f i
hforget = unwrapNT $ hcata hforgetAlg