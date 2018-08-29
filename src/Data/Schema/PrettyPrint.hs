{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
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
newtype SchemaDoc a = SchemaDoc { getDoc :: AnsiDoc } deriving Functor

class ToSchemaDoc s where
  toSchemaDoc :: s ~> SchemaDoc

instance (ToSchemaDoc p, ToSchemaDoc q) => ToSchemaDoc (Sum p q) where
  toSchemaDoc (InL l) = toSchemaDoc l
  toSchemaDoc (InR r) = toSchemaDoc r

toSchemaDocAlg :: ToSchemaDoc s => HAlgebra (SchemaF s) SchemaDoc
toSchemaDocAlg = wrapNT $ \case
  PrimitiveSchema p   -> SchemaDoc $ PP.colon <+> (getDoc $ toSchemaDoc p)
  SeqSchema elemDoc   -> SchemaDoc $ PP.colon <+> PP.vsep [PP.lbracket, getDoc elemDoc, PP.rbracket]
  RecordSchema fields -> SchemaDoc . renderFields $ ST.execState (runAp fieldDoc fields) []
    where fieldDoc :: FieldDef o SchemaDoc v -> State [AnsiDoc] v
          fieldDoc (FieldDef name doc _) = do
            fieldDesc <- pure $ PP.pretty "*" <+> (PP.pretty name) <> (getDoc doc)
            ST.modify $ \xs -> fieldDesc:xs
            return undefined

          renderFields :: [AnsiDoc] -> AnsiDoc
          renderFields [] = PP.emptyDoc
          renderFields xs = PP.nest 2 $ PP.line <> PP.vsep xs
  UnionSchema alts -> SchemaDoc $ PP.indent 2 $ PP.vsep $ altDoc <$> alts
    where altDoc :: AltDef SchemaDoc a -> AnsiDoc
          altDoc (AltDef name (SchemaDoc doc) _) = PP.pretty "-"
            <+> (PP.pretty name)
            <>  doc
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
  PrimitiveSchema p   -> SchemaLayout $ \x -> PP.colon <+> runSchemaLayout (toSchemaLayout p) x
  SeqSchema elemLay -> SchemaLayout $ \xs -> PP.colon <+> (PP.vsep $ Vector.toList $ fmap (runSchemaLayout elemLay) xs)
  RecordSchema fields -> SchemaLayout $ \rc -> renderFields $ ST.execState (runAp (fieldDocOf rc) fields) []
    where fieldDocOf :: o -> FieldDef o SchemaLayout v -> State [AnsiDoc] v
          fieldDocOf obj (FieldDef name (SchemaLayout layout) getter) = do
            let el = view getter obj
            fieldDesc <- pure $ PP.pretty "*" <+> (PP.pretty name) <> (layout el)
            ST.modify $ \xs -> fieldDesc:xs
            return el

          renderFields :: [AnsiDoc] -> AnsiDoc
          renderFields [] = PP.emptyDoc
          renderFields xs = PP.nest 2 $ PP.line <> PP.vsep xs
  UnionSchema alts -> SchemaLayout $ \value -> head . catMaybes $ fmap (layoutAlt value) alts
    where layoutAlt :: o -> AltDef SchemaLayout o -> Maybe AnsiDoc
          layoutAlt obj (AltDef name (SchemaLayout layout) getter) = do
            doc <- layout <$> obj ^? getter
            return $ PP.pretty "-" <+> (PP.pretty name) <> doc
  IsoSchema (SchemaLayout baseLayout) iso -> SchemaLayout $ \value -> baseLayout (view (re iso) value)

instance ToSchemaLayout s => ToSchemaLayout (Schema ann s) where
  toSchemaLayout schema = (cataNT toSchemaLayoutAlg) (hforget schema)

prettyPrinter :: ToSchemaLayout s => s a -> (a -> IO ())
prettyPrinter schema = \x -> do
  PP.putDoc $ runSchemaLayout (toSchemaLayout schema) x
  putStrLn ""