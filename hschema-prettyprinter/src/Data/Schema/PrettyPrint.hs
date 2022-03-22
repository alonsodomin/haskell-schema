{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Schema.PrettyPrint
     ( module Data.Schema.PrettyPrint.Internal.Algebra
     , module Data.Schema.PrettyPrint.Internal.Types
     , putSchema
     , putSchema'
     , prettyPrinter
     , prettyPrinter'
     ) where


import           Data.Schema.PrettyPrint.Internal.Algebra
import           Data.Schema.PrettyPrint.Internal.Types
import qualified Prettyprinter.Render.Terminal            as PP


-- | Renders the given schema to the standard out
putSchema :: ToSchemaDoc s => LayoutSettings -> s a -> IO ()
putSchema settings schema = do
  PP.putDoc . getDoc $ toSchemaDoc settings schema
  putStrLn ""

putSchema' :: ToSchemaDoc s => s a -> IO ()
putSchema' = putSchema defaultLayoutSettings

-- | Generates a renderer of data types based on the given schema
prettyPrinter :: ToSchemaLayout s => LayoutSettings -> s a -> (a -> IO ())
prettyPrinter settings schema x = do
  PP.putDoc $ renderSchemaLayout (toSchemaLayout settings schema) x
  putStrLn ""

prettyPrinter' :: ToSchemaLayout s => s a -> (a -> IO ())
prettyPrinter' = prettyPrinter defaultLayoutSettings
