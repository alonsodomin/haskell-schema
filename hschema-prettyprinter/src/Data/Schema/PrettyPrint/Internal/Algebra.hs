{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Schema.PrettyPrint.Internal.Algebra (
       SchemaDoc (..)
     , ToSchemaDoc (..)
     , toSchemaDoc'
     , SchemaLayout (..)
     , ToSchemaLayout (..)
     , toSchemaLayout'
     ) where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                           hiding (iso)
import           Control.Monad.State                    (State)
import qualified Control.Monad.State                    as ST
import           Control.Natural
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Functor.Sum
import           Data.List.NonEmpty                     (NonEmpty)
import qualified Data.List.NonEmpty                     as NEL
import           Data.Maybe
import           Data.Schema.Internal.Types
import           Data.Schema.PrettyPrint.Internal.Types
import           Prettyprinter                          ((<+>), (<>))
import qualified Prettyprinter                          as PP

renderFields :: forall o s. LayoutSettings -> (forall v. FieldDef o s v -> AnsiDoc) -> Fields s o -> AnsiDoc
renderFields settings f fields = packFields $ ST.execState (runAp fieldDoc $ unwrapField fields) []
  where fieldDoc :: FieldDef o s v -> State [AnsiDoc] v
        fieldDoc fld = do
          let fieldDesc = PP.pretty (layoutProductItem settings) <+> PP.pretty (fieldName fld) <+> f fld
          ST.modify $ \xs -> fieldDesc:xs
          return undefined

        packFields :: [AnsiDoc] -> AnsiDoc
        packFields [] = PP.emptyDoc
        packFields xs = PP.nest (layoutIndent settings) $ PP.line <> PP.vsep xs

renderAlts :: forall s o. LayoutSettings -> (AltDef s o -> Maybe AnsiDoc) -> NonEmpty (AltDef s o) -> [AnsiDoc]
renderAlts settings f alts = catMaybes . NEL.toList $ altDoc <$> alts
  where altDoc :: AltDef s o -> Maybe AnsiDoc
        altDoc a = (\x -> PP.indent (layoutIndent settings) $ PP.pretty (layoutSumItem settings) <+> PP.pretty (altName a) <> x) <$> f a

-- | Defines the transformation of schema `s a` into a `SchemaDoc a`
class ToSchemaDoc s where
  toSchemaDoc :: LayoutSettings -> s ~> SchemaDoc

toSchemaDoc' :: ToSchemaDoc s => s ~> SchemaDoc
toSchemaDoc' = toSchemaDoc defaultLayoutSettings

instance (ToSchemaDoc p, ToSchemaDoc q) => ToSchemaDoc (Sum p q) where
  toSchemaDoc settings (InL l) = toSchemaDoc settings l
  toSchemaDoc settings (InR r) = toSchemaDoc settings r

toSchemaDocAlg :: ToSchemaDoc s => LayoutSettings -> HAlgebra (SchemaF s) SchemaDoc
toSchemaDocAlg settings = wrapNT $ \case
  PrimitiveSchema p   -> SchemaDoc $ PP.pretty (layoutTypeAssignment settings) <+> getDoc (toSchemaDoc settings p)
  RecordSchema fields -> SchemaDoc $ renderFields settings fieldDoc' fields
    where fieldDoc' :: FieldDef o SchemaDoc v -> AnsiDoc
          fieldDoc' (RequiredField _ schemaDoc _) = getDoc schemaDoc
          fieldDoc' (OptionalField _ schemaDoc _) = PP.pretty (layoutOptional settings) <> getDoc schemaDoc
  UnionSchema alts -> SchemaDoc $ PP.vsep $ renderAlts settings altDoc' alts
    where altDoc' :: AltDef SchemaDoc a -> Maybe AnsiDoc
          altDoc' (AltDef _ (SchemaDoc doc) _) = Just doc
  AliasSchema baseDoc _ -> SchemaDoc $ getDoc baseDoc

instance ToSchemaDoc s => ToSchemaDoc (Schema s) where
  toSchemaDoc settings schema = cataNT (toSchemaDocAlg settings) (unwrapSchema schema)

newtype SchemaLayout a = SchemaLayout {
  renderSchemaLayout :: a -> AnsiDoc
  }

instance Contravariant SchemaLayout where
  contramap f (SchemaLayout g) = SchemaLayout $ g . f

instance Divisible SchemaLayout where
  conquer = SchemaLayout $ const PP.emptyDoc
  divide split leftLayout rightLayout = SchemaLayout $ \x ->
    let (left, right) = split x
        leftDoc       = renderSchemaLayout leftLayout left
        rightDoc      = renderSchemaLayout rightLayout right
    in leftDoc <+> PP.pretty "," <+> rightDoc

class ToSchemaLayout s where
  toSchemaLayout :: LayoutSettings -> s ~> SchemaLayout

toSchemaLayout' :: ToSchemaLayout s => s ~> SchemaLayout
toSchemaLayout' = toSchemaLayout defaultLayoutSettings

instance (ToSchemaLayout p, ToSchemaLayout q) => ToSchemaLayout (Sum p q) where
  toSchemaLayout settings (InL l) = toSchemaLayout settings l
  toSchemaLayout settings (InR r) = toSchemaLayout settings r

toSchemaLayoutAlg :: ToSchemaLayout s => LayoutSettings -> HAlgebra (SchemaF s) SchemaLayout
toSchemaLayoutAlg settings = wrapNT $ \case
  PrimitiveSchema p   -> SchemaLayout $ \x   -> PP.colon <+> renderSchemaLayout (toSchemaLayout settings p) x
  RecordSchema fields -> SchemaLayout $ \rc  -> renderFields settings (fieldDocOf rc) fields
    where fieldDocOf :: o -> FieldDef o SchemaLayout v -> AnsiDoc
          fieldDocOf obj (RequiredField _ (SchemaLayout layout) getter) =
            let el = view getter obj
            in layout el
          fieldDocOf obj (OptionalField _ (SchemaLayout layout) getter) =
            let el = view getter obj
            in maybe (PP.pretty $ layoutNothing settings) layout el
  UnionSchema alts -> SchemaLayout $ \value -> head $ renderAlts settings (layoutAlt' value) alts
    where layoutAlt' :: o -> AltDef SchemaLayout o -> Maybe AnsiDoc
          layoutAlt' obj (AltDef _ (SchemaLayout layout) getter) = layout <$> obj ^? getter
  AliasSchema (SchemaLayout baseLayout) getter -> SchemaLayout $ \value -> baseLayout (view (re getter) value)

instance ToSchemaLayout s => ToSchemaLayout (Schema s) where
  toSchemaLayout settings schema = cataNT (toSchemaLayoutAlg settings) (unwrapSchema schema)
