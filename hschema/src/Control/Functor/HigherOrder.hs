{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}

module Control.Functor.HigherOrder where

import           Control.Natural

type HAlgebra   f g = f g :~> g
type HCoalgebra f g = g :~> f g

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

newtype HMutu
  (f :: ((* -> *) -> * -> *))
  (g :: ((* -> *) -> * -> *))
  (a :: *) = HMutu { unmutu :: f (g (HMutu f g)) a }

instance HFunctor f => HFunctor (HEnvT f a) where
  hfmap nt = \fa -> HEnvT (hask fa) (hfmap nt (hlocal fa))

instance Functor (f g) => Functor (HEnvT f e g) where
  fmap f env = HEnvT (hask env) (fmap f (hlocal env))

type HCofree
  (f :: ((* -> *) -> * -> *))
  (a :: *) = HFix (HEnvT f a)

hcofree :: a -> f (HCofree f a) b -> HCofree f a b
hcofree a fhc = HFix (HEnvT a fhc)

cataNT :: HFunctor f => HAlgebra f g -> HFix f ~> g
cataNT alg = unwrapNT alg . nt
  where nt hf = hfmap (cataNT alg) (unfix hf)

anaNT :: HFunctor f => HCoalgebra f g -> g ~> HFix f
anaNT coalg g = HFix $ hfmap (anaNT coalg) $ unwrapNT coalg g

--hyloNT :: HFunctor f => HAlgebra f g -> HCoalgebra g f -> HFix f ~> HFix g

hforgetAlg :: HAlgebra (HEnvT f a) (HFix f)
hforgetAlg = wrapNT $ \env -> HFix $ hlocal env

hforget :: HFunctor f => HCofree f a ~> HFix f
hforget = cataNT hforgetAlg

htagCoalg :: HFunctor f => a -> HCoalgebra (HEnvT f a) (HFix f)
htagCoalg tag = wrapNT $ \hfix -> HEnvT tag (unfix hfix)

htag :: HFunctor f => a -> HFix f ~> HCofree f a
htag tag = anaNT (htagCoalg tag)
