{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Data.Schema.Internal.Types where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                hiding (iso)
import qualified Control.Lens                as Lens
import           Control.Natural
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Vector                 (Vector)
import           Prelude                     hiding (const, seq)

-- | Metadata for a field of type `a`, belonging to the data type `o` and based on schema `s`
data FieldDef o s a =
    RequiredField Text (s a) (Getter o a)
  | OptionalField Text (s a) (Getter o (Maybe a))
  -- { fieldName     :: Text
  -- , fieldSchema   :: s a
  -- , fieldAccessor :: Getter o a
  -- }

fieldName :: FieldDef o s a -> Text
fieldName (RequiredField name _ _) = name
fieldName (OptionalField name _ _) = name

fieldSchema :: FieldDef o s a -> s a
fieldSchema (RequiredField _ s _) = s
fieldSchema (OptionalField _ s _) = s

-- contraNT :: (n -> o) -> FieldDef o s ~> FieldDef n s
-- contraNT f = \case
--   RequiredField n s g -> RequiredField n s ((to f) . g)
--   OptionalField n s g -> OptionalField n s ((to f) . g)

instance Show (s a) => Show (FieldDef o s a) where
  show field = (T.unpack $ fieldName field) ++ " :: " ++ (show $ fieldSchema field)

instance Functor s => Functor (FieldDef o s) where
  fmap f (RequiredField name s acc) = RequiredField name (fmap f s) (acc . (to f))
  fmap f (OptionalField name s acc) = OptionalField name (fmap f s) (acc . (to $ fmap f))

instance HFunctor (FieldDef o) where
  hfmap nt = \case
    RequiredField name s acc -> RequiredField name (nt s) acc
    OptionalField name s acc -> OptionalField name (nt s) acc

-- | The type of a field of type `a`, belonging to the data type `o` and based on schema `s`
type Field s o a = Ap (FieldDef o s) a
-- | The set of fields for the data type `o` based on schema `s`
type Fields s o = Field s o o

-- | Metadata for an alternative of type `a` based on schema `s`
data AltDef s a = forall b. AltDef
  { altName   :: Text
  , altSchema :: s b
  , altPrism  :: Prism' a b
  }

instance HFunctor AltDef where
  hfmap nt = \(AltDef name schema pr) -> AltDef name (nt schema) pr

-- | Metadata for a schema `s` based on primitives `p` and representing type `a`
data SchemaF p s a where
  PrimitiveSchema :: p a -> SchemaF p s a
  SeqSchema       :: s a -> SchemaF p s (Vector a)
  RecordSchema    :: Fields s a -> SchemaF p s a
  UnionSchema     :: NonEmpty (AltDef s a) -> SchemaF p s a
  AliasSchema     :: s a -> Iso' a b -> SchemaF p s b

instance HFunctor (SchemaF p) where
  hfmap nt = \case
    PrimitiveSchema p    -> PrimitiveSchema p
    SeqSchema elemSch    -> SeqSchema $ nt elemSch
    RecordSchema fields  -> RecordSchema $ hoistAp (hfmap nt) fields
    UnionSchema alts     -> UnionSchema $ fmap (hfmap nt) alts
    AliasSchema base iso -> AliasSchema (nt base) iso

-- | Perform a natural transformation of the primitive algebra of the Schema
pfmap :: (p ~> q) -> SchemaF p s a -> SchemaF q s a
pfmap nt = \case
  PrimitiveSchema p    -> PrimitiveSchema (nt p)
  SeqSchema elemSch    -> SeqSchema elemSch
  RecordSchema fields  -> RecordSchema fields
  UnionSchema alts     -> UnionSchema alts
  AliasSchema base iso -> AliasSchema base iso

-- | The Schema type itself for a set of primitives `p` and annotated with `ann`
type Schema ann p = HCofree (SchemaF p) ann
-- | Schema for the set of primitives `p` without annotations
type Schema' p = Schema () p

class HasSchema a where
  type Ann a :: *
  type PrimitivesOf a :: * -> *

  getSchema :: Schema (Ann a) (PrimitivesOf a) a
