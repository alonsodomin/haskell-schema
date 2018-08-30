{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Schema.PrettyPrint
     ( SchemaDoc (..)
     , ToSchemaDoc (..)
     , putSchema
     , SchemaLayout (..)
     , ToSchemaLayout (..)
     , prettyPrinter
     ) where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                              hiding (iso)
import           Control.Monad.State                       (State)
import qualified Control.Monad.State                       as ST
import           Control.Natural
import           Data.Functor.Sum
import           Data.Maybe
import           Data.Schema.Types
import           Data.Text.Prettyprint.Doc                 ((<+>), (<>))
import qualified Data.Text.Prettyprint.Doc                 as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP
import qualified Data.Vector                               as Vector

type AnsiDoc = PP.Doc PP.AnsiStyle

indentAmount = 2

doubleColon :: AnsiDoc
doubleColon = PP.colon <> PP.colon

layoutFields :: forall o s. (forall v. FieldDef o s v -> AnsiDoc) -> Fields s o -> AnsiDoc
layoutFields f fields = renderFields $ ST.execState (runAp fieldDoc fields) []
  where fieldDoc :: FieldDef o s v -> State [AnsiDoc] v
        fieldDoc fld = do
          fieldDesc <- pure $ PP.pretty "*" <+> (PP.pretty $ fieldName fld) <+> (f fld)
          ST.modify $ \xs -> fieldDesc:xs
          return undefined

        renderFields :: [AnsiDoc] -> AnsiDoc
        renderFields [] = PP.emptyDoc
        renderFields xs = PP.nest indentAmount $ PP.line <> PP.vsep xs

layoutAlts :: forall s o. (AltDef s o -> Maybe AnsiDoc) -> [AltDef s o] -> [AnsiDoc]
layoutAlts f alts = catMaybes $ altDoc <$> alts
  where altDoc :: AltDef s o -> Maybe AnsiDoc
        altDoc a = (\x -> PP.indent indentAmount $ PP.pretty "-" <+> (PP.pretty $ altName a) <> x) <$> (f a)

newtype SchemaDoc a = SchemaDoc { getDoc :: AnsiDoc } deriving Functor

class ToSchemaDoc s where
  toSchemaDoc :: s ~> SchemaDoc

instance (ToSchemaDoc p, ToSchemaDoc q) => ToSchemaDoc (Sum p q) where
  toSchemaDoc (InL l) = toSchemaDoc l
  toSchemaDoc (InR r) = toSchemaDoc r

toSchemaDocAlg :: ToSchemaDoc s => HAlgebra (SchemaF s) SchemaDoc
toSchemaDocAlg = wrapNT $ \case
  PrimitiveSchema p   -> SchemaDoc $ doubleColon <+> (getDoc $ toSchemaDoc p)
  SeqSchema elemDoc   -> SchemaDoc $ doubleColon <+> PP.vsep [PP.lbracket, getDoc elemDoc, PP.rbracket]
  RecordSchema fields -> SchemaDoc $ layoutFields fieldDoc' fields
    where fieldDoc' :: FieldDef o SchemaDoc v -> AnsiDoc
          fieldDoc' (FieldDef _ schemaDoc _) = getDoc schemaDoc
  UnionSchema alts -> SchemaDoc $ PP.vsep $ layoutAlts altDoc' alts
    where altDoc' :: AltDef SchemaDoc a -> Maybe AnsiDoc
          altDoc' (AltDef _ (SchemaDoc doc) _) = Just doc
  IsoSchema baseDoc _ -> SchemaDoc $ getDoc baseDoc

instance ToSchemaDoc s => ToSchemaDoc (Schema ann s) where
  toSchemaDoc schema = (cataNT toSchemaDocAlg) (hforget schema)

putSchema :: ToSchemaDoc s => s a -> IO ()
putSchema schema = do
  PP.putDoc . getDoc $ toSchemaDoc schema
  putStrLn ""

newtype SchemaLayout a = SchemaLayout { runSchemaLayout :: a -> AnsiDoc }

class ToSchemaLayout s where
  toSchemaLayout :: s ~> SchemaLayout

instance (ToSchemaLayout p, ToSchemaLayout q) => ToSchemaLayout (Sum p q) where
  toSchemaLayout (InL l) = toSchemaLayout l
  toSchemaLayout (InR r) = toSchemaLayout r

toSchemaLayoutAlg :: ToSchemaLayout s => HAlgebra (SchemaF s) SchemaLayout
toSchemaLayoutAlg = wrapNT $ \case
  PrimitiveSchema p   -> SchemaLayout $ \x  -> PP.colon <+> runSchemaLayout (toSchemaLayout p) x
  SeqSchema elemLay   -> SchemaLayout $ \xs -> PP.colon <> PP.line <> PP.indent indentAmount (PP.vsep $ Vector.toList $ fmap (runSchemaLayout elemLay) xs)
  RecordSchema fields -> SchemaLayout $ \rc -> layoutFields (fieldDocOf rc) fields
    where fieldDocOf :: o -> FieldDef o SchemaLayout v -> AnsiDoc
          fieldDocOf obj (FieldDef _ (SchemaLayout layout) getter) =
            let el = view getter obj
            in layout el
  UnionSchema alts -> SchemaLayout $ \value ->  (head $ layoutAlts (layoutAlt' value) alts)
    where layoutAlt' :: o -> AltDef SchemaLayout o -> Maybe AnsiDoc
          layoutAlt' obj (AltDef _ (SchemaLayout layout) getter) = layout <$> obj ^? getter
  IsoSchema (SchemaLayout baseLayout) getter -> SchemaLayout $ \value -> baseLayout (view (re getter) value)

instance ToSchemaLayout s => ToSchemaLayout (Schema ann s) where
  toSchemaLayout schema = (cataNT toSchemaLayoutAlg) (hforget schema)

prettyPrinter :: ToSchemaLayout s => s a -> (a -> IO ())
prettyPrinter schema = \x -> do
  PP.putDoc $ runSchemaLayout (toSchemaLayout schema) x
  putStrLn ""