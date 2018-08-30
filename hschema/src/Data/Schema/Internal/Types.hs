{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Data.Schema.Internal.Types where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                hiding (iso)
import qualified Control.Lens                as Lens
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as Vector
import           Prelude                     hiding (const, seq)

-- | Metadata for a field of type `a`, belonging to the data type `o` and based on schema `s`
data FieldDef o s a = FieldDef
  { fieldName     :: Text
  , fieldSchema   :: s a
  , fieldAccessor :: Getter o a
  }

instance Show (s a) => Show (FieldDef o s a) where
  show f = (T.unpack $ fieldName f) ++ " :: " ++ (show $ fieldSchema f)

instance HFunctor (FieldDef o) where
  hfmap nt = \(FieldDef name sch acc) -> FieldDef name (nt sch) acc

-- | The type of a field of type `a`, belonging to the data type `o` and based on schema `s`
type Field s o a = Ap (FieldDef o s) a
-- | The set of fields for the data type `o` based on schema `s`
type Fields s o = Field s o o

-- | Define a field
field :: Text -> s a -> Getter o a -> Field s o a
field name schema getter = liftAp (FieldDef name schema getter)

-- | Metadata for an alternative of type `a` based on schema `s`
data AltDef s a = forall b. AltDef
  { altName   :: Text
  , altSchema :: s b
  , altPrism  :: Prism' a b
  }

instance HFunctor AltDef where
  hfmap nt = \(AltDef name schema pr) -> AltDef name (nt schema) pr

-- | Define an alternative
alt :: Text -> s b -> Prism' a b -> AltDef s a
alt = AltDef

-- | Metadata for a schema `s` based on primitives `p` and representing type `a`
data SchemaF p s a where
  PrimitiveSchema :: p a -> SchemaF p s a
  SeqSchema       :: s a -> SchemaF p s (Vector a)
  RecordSchema    :: Fields s a -> SchemaF p s a
  UnionSchema     :: [AltDef s a] -> SchemaF p s a
  IsoSchema       :: s a -> Iso' a b -> SchemaF p s b

instance HFunctor (SchemaF p) where
  hfmap nt = \case
    PrimitiveSchema p   -> PrimitiveSchema p
    SeqSchema elemSch   -> SeqSchema $ nt elemSch
    RecordSchema fields -> RecordSchema $ hoistAp (hfmap nt) fields
    UnionSchema alts    -> UnionSchema $ fmap (hfmap nt) alts
    IsoSchema base i    -> IsoSchema (nt base) i

-- | The Schema type itself for a set of primitives `p` and annotated with `ann`
type Schema ann p = HCofree (SchemaF p) ann
-- | Schema for the set of primitives `p` without annotations
type Schema' p = Schema () p

-- | Define an annotated schema for primitives of type `p`
prim :: ann -> p a -> Schema ann p a
prim ann primAlg = hcofree ann $ PrimitiveSchema primAlg

prim' :: p a -> Schema' p a
prim' = prim ()

-- | Define a schema for a type that is always constant
const :: ann -> a -> Schema ann p a
const ann a = hcofree ann (RecordSchema $ Pure a)

const' :: a -> Schema' p a
const' = const ()

-- | Define the schema of record using the given fields
record :: ann -> Fields (Schema ann p) a -> Schema ann p a
record ann ps = hcofree ann (RecordSchema ps)

record' :: Fields (Schema' p) a -> Schema' p a
record' = record ()

-- | Define the schema of a vector based on the element type
seq :: ann -> Schema ann p a -> Schema ann p (Vector a)
seq ann elemSchema = hcofree ann (SeqSchema elemSchema)

seq' :: Schema' p a -> Schema' p (Vector a)
seq' = seq ()

-- | Define the schema of a list based on the element type
list :: ann -> Schema ann p a -> Schema ann p [a]
list ann elemSchema = iso ann (seq ann elemSchema) (Lens.iso Vector.toList Vector.fromList)

list' :: Schema' p a -> Schema' p [a]
list' = list ()

-- | Define the schema of an union (coproduct) type based on the given alternatives
oneOf :: ann -> [AltDef (Schema ann p) a] -> Schema ann p a
oneOf ann alts = hcofree ann (UnionSchema alts)

oneOf' :: [AltDef (Schema' p) a] -> Schema' p a
oneOf' = oneOf ()

-- | Define an schema that is isomorphic to another one using the given ISO transformation
iso :: ann -> Schema ann p a -> Iso' a b -> Schema ann p b
iso ann base i = hcofree ann (IsoSchema base i)

iso' :: Schema' p a -> Iso' a b -> Schema' p b
iso' = iso ()
