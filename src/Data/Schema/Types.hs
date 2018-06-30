{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}

module Data.Schema.Types where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                hiding (iso)
import           Data.Text                   (Text)
import           Data.Vector                 (Vector)
import           Prelude                     hiding (const, seq)

data FieldDef o s a = FieldDef
  { fieldName     :: Text
  , fieldSchema   :: s a
  , fieldAccessor :: Getter o a
  }

instance HFunctor (FieldDef o) where
  hfmap nt = \(FieldDef name sch acc) -> FieldDef name (nt sch) acc

type Field s o a = Ap (FieldDef o s) a
type Fields s o = Field s o o

prop :: Text -> s a -> Getter o a -> Field s o a
prop name schema getter = liftAp (FieldDef name schema getter)

data AltDef s a = forall b. AltDef
  { altName   :: Text
  , altSchema :: s b
  , altPrism  :: Prism' a b
  }

instance HFunctor AltDef where
  hfmap nt = \(AltDef name schema pr) -> AltDef name (nt schema) pr

alt :: Text -> s b -> Prism' a b -> AltDef s a
alt = AltDef

data SchemaF p s a where
  PrimitiveSchema :: p a -> SchemaF p s a
  SeqSchema       :: s a -> SchemaF p s (Vector a)
  RecordSchema    :: Fields s a -> SchemaF p s a
  UnionSchema     :: [AltDef s a] -> SchemaF p s a
  IsoSchema       :: s a -> Iso' a b -> SchemaF p s b

type Schema ann p = HCofree (SchemaF p) ann
type Schema' p = Schema () p

instance HFunctor (SchemaF p) where
  hfmap nt = \case
    PrimitiveSchema p   -> PrimitiveSchema p
    SeqSchema elemSch   -> SeqSchema $ nt elemSch
    RecordSchema fields -> RecordSchema $ hoistAp (hfmap nt) fields
    UnionSchema alts    -> UnionSchema $ fmap (hfmap nt) alts
    IsoSchema base i    -> IsoSchema (nt base) i

const :: ann -> a -> Schema ann p a
const ann a = hcofree ann (RecordSchema $ Pure a)

const' :: a -> Schema' p a
const' = const ()

record :: ann -> Fields (Schema ann p) a -> Schema ann p a
record ann ps = hcofree ann (RecordSchema ps)

record' :: Fields (Schema' p) a -> Schema' p a
record' = record ()

seq :: ann -> Schema ann p a -> Schema ann p (Vector a)
seq ann elemSchema = hcofree ann (SeqSchema elemSchema)

seq' :: Schema' p a -> Schema' p (Vector a)
seq' = seq ()

union :: ann -> [AltDef (Schema ann p) a] -> Schema ann p a
union ann alts = hcofree ann (UnionSchema alts)

union' :: [AltDef (Schema' p) a] -> Schema' p a
union' = union ()

iso :: ann -> Schema ann p a -> Iso' a b -> Schema ann p b
iso ann base i = hcofree ann (IsoSchema base i)

iso' :: Schema' p a -> Iso' a b -> Schema' p b
iso' = iso ()
