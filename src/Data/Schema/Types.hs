{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}

module Data.Schema.Types where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens
import           Control.Natural
import           Data.Text                   (Text)
import           Data.Vector                 (Vector)
import           Prelude                     hiding (const, seq)

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

data SchemaF p s a where
  PrimitiveSchema :: p a -> SchemaF p s a
  SeqSchema :: s a -> SchemaF p s (Vector a)
  RecordSchema :: Props s a -> SchemaF p s a
  UnionSchema :: [AltDef s a] -> SchemaF p s a

type Schema_ p = HFix (SchemaF p)
type Schema ann p = HCofree (SchemaF p) ann

instance HFunctor (SchemaF p) where
  hfmap nt = wrapNT $ \fa -> unwrapNT (hfmap nt) fa

const :: ann -> a -> Schema ann p a
const ann a = hcofree ann (RecordSchema $ Pure a)

record :: ann -> Props (Schema ann p) a -> Schema ann p a
record ann ps = hcofree ann (RecordSchema ps)

seq :: ann -> Schema ann p a -> Schema ann p (Vector a)
seq ann elemSchema = hcofree ann (SeqSchema elemSchema)

union :: ann -> [AltDef (Schema ann p) a] -> Schema ann p a
union ann alts = hcofree ann (UnionSchema alts)
