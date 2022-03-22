{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Schema.PrettyPrint.Internal.Types where

import           Control.Natural
import           Data.Functor.Sum
import           Data.Text
import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Terminal as PP

type AnsiDoc = PP.Doc PP.AnsiStyle

-- | General settings to be used when rendering documents
data LayoutSettings = LayoutSettings {
    layoutIndent         :: !Int
  , layoutTypeAssignment :: !Text
  , layoutOptional       :: !Text
  , layoutSumItem        :: !Text
  , layoutProductItem    :: !Text
  , layoutNothing        :: !Text
  }

-- | Default layout settings
defaultLayoutSettings :: LayoutSettings
defaultLayoutSettings = LayoutSettings {
    layoutIndent = 2
  , layoutTypeAssignment = pack "::"
  , layoutOptional = pack "?"
  , layoutSumItem = pack "+"
  , layoutProductItem = pack "*"
  , layoutNothing = pack "Nothing"
  }

-- | Functor-like structure holding a document for a schema
newtype SchemaDoc a = SchemaDoc { getDoc :: AnsiDoc } deriving Functor

instance Applicative SchemaDoc where
  pure _ = SchemaDoc PP.emptyDoc
  (SchemaDoc l) <*> (SchemaDoc r) = SchemaDoc $ l <> r
