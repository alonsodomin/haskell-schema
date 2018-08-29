{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Schema.PrettyPrint where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Monad.State                       (State)
import qualified Control.Monad.State                       as ST
import           Control.Natural
import           Data.Schema.Types
import           Data.Text                                 (Text)
import           Data.Text.Prettyprint.Doc                 ((<+>), (<>))
import qualified Data.Text.Prettyprint.Doc                 as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP

type AnsiDoc = PP.Doc PP.AnsiStyle
newtype Doc a = MkDoc { getDoc :: AnsiDoc } deriving Functor

class ToDoc s where
  toDoc :: s ~> Doc

toDocAlg :: ToDoc s => HAlgebra (SchemaF s) Doc
toDocAlg = wrapNT $ \case
  PrimitiveSchema p   -> MkDoc $ PP.colon <+> (getDoc $ toDoc p)
  SeqSchema elemDoc   -> MkDoc $ PP.colon <+> PP.vsep [PP.lbracket, getDoc elemDoc, PP.rbracket]
  RecordSchema fields -> MkDoc . renderFields $ ST.execState (runAp fieldDoc fields) []
    where fieldDoc :: FieldDef o Doc v -> State [AnsiDoc] v
          fieldDoc (FieldDef name doc _) = do
            fieldDesc <- pure $ PP.pretty "*" <+> (PP.pretty name) <> (getDoc doc)
            ST.modify $ \xs -> fieldDesc:xs
            return undefined

          renderFields :: [AnsiDoc] -> AnsiDoc
          renderFields [] = PP.emptyDoc
          renderFields xs = PP.nest 2 $ PP.line <> PP.vsep xs
  UnionSchema alts -> MkDoc $ PP.indent 2 $ PP.vsep $ altDoc <$> alts
    where altDoc :: AltDef Doc a -> AnsiDoc
          altDoc (AltDef name (MkDoc doc) _) = PP.pretty "-"
            <+> (PP.pretty name)
            <>  doc
  IsoSchema baseDoc _ -> MkDoc $ getDoc baseDoc

instance ToDoc s => ToDoc (Schema ann s) where
  toDoc schema = (cataNT toDocAlg) (hforget schema)

putSchema :: ToDoc s => s a -> IO ()
putSchema schema = do
  PP.putDoc . getDoc $ toDoc schema
  putStrLn ""
