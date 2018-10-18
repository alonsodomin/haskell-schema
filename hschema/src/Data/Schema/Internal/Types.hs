{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Data.Schema.Internal.Types where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                hiding (iso)
import qualified Control.Lens                as Lens
import           Control.Natural
import           Data.Functor.Invariant
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Profunctor
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Vector                 (Vector)
import           Prelude                     hiding (const, seq)

-- | Metadata for a field of type `a`, belonging to the data type `o` and based on schema `s`
data FieldDef o s a where
  RequiredField :: Text -> s a -> Getter o a -> FieldDef o s a
  OptionalField :: Text -> s a -> Getter o (Maybe a) -> FieldDef o s (Maybe a)

fieldName :: FieldDef o s a -> Text
fieldName (RequiredField name _ _) = name
fieldName (OptionalField name _ _) = name

instance HFunctor (FieldDef o) where
  hfmap nt = \case
    RequiredField name s acc -> RequiredField name (nt s) acc
    OptionalField name s acc -> OptionalField name (nt s) acc

-- | The type of a field of type `a`, belonging to the data type `o` and based on schema `s`
newtype Field s o a = Field { unwrapField :: Ap (FieldDef o s) a }

hoistField :: (m ~> n) -> Field m o a -> Field n o a
hoistField nt (Field ap) = Field $ hoistAp (hfmap nt) ap

-- | The set of fields for the data type `o` based on schema `s`
type Fields s o = Field s o o

instance Functor (Field s o) where
  fmap f (Field x) = Field $ fmap f x

instance Applicative (Field s o) where
  pure x = Field $ Pure x
  (Field x) <*> (Field y) = Field (x <*> y)

instance Profunctor (Field s) where
  lmap f (Field ap) = Field $ hoistAp (contraNT f) ap
    where contraNT :: (n -> o) -> FieldDef o s ~> FieldDef n s
          contraNT f = \case
            RequiredField n s g -> RequiredField n s ((to f) . g)
            OptionalField n s g -> OptionalField n s ((to f) . g)
  rmap = fmap

-- | Define a field
field :: Text -> s a -> Getter o a -> Field s o a
field name schema getter = Field $ liftAp (RequiredField name schema getter)

optional :: Text -> s a -> Getter o (Maybe a) -> Field s o (Maybe a)
optional name schema getter = Field $ liftAp (OptionalField name schema getter)

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
  RecordSchema    :: Fields s a -> SchemaF p s a
  UnionSchema     :: NonEmpty (AltDef s a) -> SchemaF p s a
  AliasSchema     :: s a -> Iso' a b -> SchemaF p s b

instance HFunctor (SchemaF p) where
  hfmap nt = \case
    PrimitiveSchema p         -> PrimitiveSchema p
    RecordSchema (Field flds) -> RecordSchema . Field $ hoistAp (hfmap nt) flds
    UnionSchema alts          -> UnionSchema $ fmap (hfmap nt) alts
    AliasSchema base iso      -> AliasSchema (nt base) iso

-- | The Schema type itself for a set of primitives `p`
newtype Schema p a = Schema { unwrapSchema :: HFix (SchemaF p) a }

instance Invariant (Schema p) where
  invmap f g sch = case (unfix . unwrapSchema $ sch) of
    AliasSchema base iso -> Schema . HFix $ AliasSchema base (iso . (Lens.iso f g))
    otherwise            -> Schema . HFix $ AliasSchema (unwrapSchema sch) (Lens.iso f g)

-- | An Schema has a HFunctor that performs a natural transformation of the primitive algebra of the Schema
instance HFunctor Schema where
  hfmap nt (Schema fsch) = Schema $ cataNT pfmapAlg fsch
    where pfmapAlg = wrapNT $ \sch -> HFix $ pfmap nt sch

          pfmap :: (p ~> q) -> SchemaF p s ~> SchemaF q s
          pfmap nt = \case
            PrimitiveSchema p    -> PrimitiveSchema (nt p)
            RecordSchema fields  -> RecordSchema fields
            UnionSchema alts     -> UnionSchema alts
            AliasSchema base iso -> AliasSchema base iso

class HasSchema a where
  type PrimitivesOf a :: * -> *

  getSchema :: Schema (PrimitivesOf a) a
