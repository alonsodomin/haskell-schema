{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}

module Data.Schema.Types where

import           Control.Applicative.Free
import Control.Functor.HigherOrder
import           Control.Lens
import           Data.Text                (Text)
import           Data.Vector              (Vector)
import           Prelude                  hiding (const, seq)

data PropDef s o a = PropDef
  { propName     :: Text
  , propSchema   :: s a
  , propAccessor :: Getter o a
  }

type Prop s o a = Ap (PropDef s o) a
type Props s o = Prop s o o

prop :: Text -> s a -> Getter o a -> Prop s o a
prop name schema getter = liftAp (PropDef name schema getter)

data AltDef s a = forall b. AltDef
  { altName   :: Text
  , altSchema :: s b
  , altPrism  :: Prism' a b
  }

-- instance Functor p => Functor (AltDef p) where
--   fmap f (AltDef _ schema)

alt :: Text -> s b -> Prism' a b -> AltDef s a
alt = AltDef

data SchemaF p f a where
  PrimitiveSchema :: p a -> SchemaF p f a
  SeqSchema :: f a -> SchemaF p f (Vector a)
  RecordSchema :: Props f a -> SchemaF p f a
  UnionSchema :: [AltDef f a] -> SchemaF p f a

type Schema p = HFix (SchemaF p)
type AnnSchema p a = HCofree a (SchemaF p)

const :: a -> Schema p a
const a = HFix . RecordSchema $ Pure a

-- record :: Props s a -> s a
-- record = HFix . RecordSchema

seq :: Schema p a -> Schema p (Vector a)
seq = HFix . SeqSchema

-- union :: [AltDef p a] -> Schema p a
-- union = HFix . UnionSchema
