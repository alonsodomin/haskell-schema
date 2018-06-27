{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Control.Functor.HigherOrder where

import           Control.Natural

type HAlgebra f g = f g :~> g

class HFunctor (f :: ((* -> *) -> * -> *)) where
  hfmap :: (m ~> n) -> f m ~> f n

class HFunctor f => HPointed f where
  hreturn :: Functor g => g a -> f g a

class HFunctor f => HCopointed f where
  hextract :: Functor g => f g a -> g a

newtype HFix f a = HFix { unfix :: f (HFix f) a }

data HEnvT
  (f :: ((* -> *) -> * -> *))
  (e :: *)
  (g :: (* -> *))
  (i :: *) = HEnvT { hask :: !e, hlocal :: f g i }

instance HFunctor f => HFunctor (HEnvT f a) where
  hfmap nt = \fm -> HEnvT (hask fm) ((hfmap nt) (hlocal fm))

type HCofree
  (f :: ((* -> *) -> * -> *))
  (a :: *) = HFix (HEnvT f a)

hcofree :: a -> f (HCofree f a) b -> HCofree f a b
hcofree a fhc = HFix (HEnvT a fhc)

-- instance HFunctor f => Functor (HCofree f a) where
--   fmap f fa =
--     let env = unfix fa
--         --hf :: HCofree f a :~> HCofree f b
--         hf = wrapNT $ \gcf -> fmap f gcf
--     in hcofree (f $ hask env) ((unwrapNT $ hfmap hf) fa)

cataNT :: HFunctor f => HAlgebra f g -> HFix f ~> g
cataNT alg = (unwrapNT alg) . nt
  where nt hf = (hfmap (cataNT alg)) (unfix hf)

hforgetAlg :: HAlgebra (HEnvT f a) (HFix f)
hforgetAlg = wrapNT $ \env -> HFix $ hlocal env

hforget :: HFunctor f => HCofree f a ~> HFix f
hforget = cataNT hforgetAlg
