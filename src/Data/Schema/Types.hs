{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}

module Data.Schema.Types where

import           Control.Applicative.Free
import           Control.Lens
import           Data.Text                (Text)
import           Data.Vector              (Vector)
import           Prelude                  hiding (const, seq)

data PropDef p o a = PropDef
  { propName     :: Text
  , propSchema   :: Schema p a
  , propAccessor :: Getter o a
  }

type Prop p o a = Ap (PropDef p o) a
type Props p o = Prop p o o

prop :: Text -> Schema p a -> Getter o a -> Prop p o a
prop name schema getter = liftAp (PropDef name schema getter)

data AltDef p a = forall b. AltDef
  { altName   :: Text
  , altSchema :: Schema p b
  , altPrism  :: Prism' a b
  }

-- instance Functor p => Functor (AltDef p) where
--   fmap f (AltDef _ schema)

alt :: Text -> Schema p b -> Prism' a b -> AltDef p a
alt = AltDef

data Schema p a where
  -- IntSchema :: Schema Int
  -- BoolSchema :: Schema Bool
  -- StringSchema :: Schema Text
  PrimitiveSchema :: p a -> Schema p a
  SeqSchema :: Schema p a -> Schema p (Vector a)
  RecordSchema :: Props p a -> Schema p a
  UnionSchema :: [AltDef p a] -> Schema p a

-- instance Functor p => Functor (Schema p) where
--   fmap f (PrimitiveSchema p) = PrimitiveSchema (fmap f p)
--   fmap f (SeqSchema elemSchema) = SeqSchema (fmap f elemSchema)
--   fmap f (RecordSchema props) = RecordSchema (fmap f props)
--   fmap f (UnionSchema alts) = UnionSchema (fmap f alts)

const :: a -> Schema p a
const a = RecordSchema $ Pure a

record :: Props p a -> Schema p a
record = RecordSchema

seq :: Schema p a -> Schema p (Vector a)
seq = SeqSchema

union :: [AltDef p a] -> Schema p a
union = UnionSchema
