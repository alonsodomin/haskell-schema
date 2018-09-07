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
import           Data.Hashable
import Data.Profunctor
import           Data.HashMap.Strict         (HashMap)
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Vector                 (Vector)
import           Prelude                     hiding (const, seq)

-- | Metadata for a field of type `a`, belonging to the data type `o` and based on schema `s`
data FieldDef o s a where
  RequiredField :: Text -> s a -> Getter o a -> FieldDef o s a
  OptionalField :: Text -> s a -> Getter o (Maybe a) -> FieldDef o s (Maybe a)
  -- = FieldDef
  -- { fieldName     :: Text
  -- , fieldSchema   :: s a
  -- , fieldAccessor :: Getter o a
  -- }

fieldName :: FieldDef o s a -> Text
fieldName (RequiredField name _ _) = name
fieldName (OptionalField name _ _) = name

-- fieldSchema :: FieldDef o s a -> s a
-- fieldSchema (RequiredField _ schema _) = schema
-- fieldSchema (OptionalField _ schema _) = schema

readField :: FieldDef o s a -> o -> a
readField (RequiredField _ _ getter) o = o ^. getter
readField (OptionalField _ _ getter) o = o ^. getter

getterF :: Functor f => (a -> b) -> Getter (f a) (f b)
getterF f = to (fmap f)

instance Functor s => Functor (FieldDef o s) where
  fmap f = \case
    RequiredField name s acc -> RequiredField name (fmap f s) (acc . (to f))
    OptionalField name s acc -> OptionalField name s acc

instance HFunctor (FieldDef o) where
  hfmap nt = \case
    RequiredField name s acc -> RequiredField name (nt s) acc
    OptionalField name s acc -> OptionalField name (nt s) acc

-- | The type of a field of type `a`, belonging to the data type `o` and based on schema `s`
newtype Field s o a = Field { toFieldAp :: Ap (FieldDef o s) a }
-- | The set of fields for the data type `o` based on schema `s`
type Fields s o = Field s o o

instance Functor (Field s o) where
  fmap f (Field x) = Field $ fmap f x

instance Applicative (Field s o) where
  pure x = Field $ Pure x
  (Field x) <*> (Field y) = Field (x <*> y)

instance Profunctor (Field s) where
  lmap f (Field x) = Field $ hoistAp (contraNT f) x
    where contraNT :: (n -> o) -> FieldDef o s ~> FieldDef n s
          contraNT f = \case
            RequiredField n s g -> RequiredField n s ((to f) . g)
            OptionalField n s g -> OptionalField n s ((to f) . g)
  rmap = fmap

-- | Define a field
field :: Text -> s a -> Getter o a -> Field s o a
field name schema getter = Field $ liftAp (RequiredField name schema getter)

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
  OptSchema       :: s a -> SchemaF p s (Maybe a)
  SeqSchema       :: s a -> SchemaF p s (Vector a)
  HashSchema      :: (Hashable k, Eq k) => s k -> s a -> SchemaF p s (HashMap k a)
  RecordSchema    :: Fields s a -> SchemaF p s a
  UnionSchema     :: NonEmpty (AltDef s a) -> SchemaF p s a
  AliasSchema     :: s a -> Iso' a b -> SchemaF p s b

instance HFunctor (SchemaF p) where
  hfmap nt = \case
    PrimitiveSchema p         -> PrimitiveSchema p
    OptSchema base            -> OptSchema $ nt base
    SeqSchema elemSch         -> SeqSchema $ nt elemSch
    HashSchema keySch elemSch -> HashSchema (nt keySch) (nt elemSch)
    RecordSchema (Field flds) -> RecordSchema . Field $ hoistAp (hfmap nt) flds
    UnionSchema alts          -> UnionSchema $ fmap (hfmap nt) alts
    AliasSchema base iso      -> AliasSchema (nt base) iso

-- | Perform a natural transformation of the primitive algebra of the Schema
pfmap :: (p ~> q) -> SchemaF p s a -> SchemaF q s a
pfmap nt = \case
  PrimitiveSchema p    -> PrimitiveSchema (nt p)
  OptSchema base       -> OptSchema base
  SeqSchema elemSch    -> SeqSchema elemSch
  HashSchema key el    -> HashSchema key el
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
