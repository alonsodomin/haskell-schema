{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Schema.QuickCheck where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens
import           Control.Natural
import           Data.Functor.Sum
import           Data.Schema.Internal.Types
import qualified Data.Vector                 as Vector
import           Test.QuickCheck

class ToGen a where
  toGen :: a ~> Gen

instance (ToGen p, ToGen q) => ToGen (Sum p q) where
  toGen (InL l) = toGen l
  toGen (InR r) = toGen r

genAlg :: ToGen p => HAlgebra (SchemaF p) Gen
genAlg = wrapNT $ \case
  PrimitiveSchema p    -> toGen p
  SeqSchema elemSchema -> Vector.fromList <$> listOf elemSchema
  RecordSchema fields  -> runAp fieldSchema fields
  UnionSchema alts     -> oneof $ fmap genAlt alts
    where genAlt :: AltDef Gen a -> Gen a
          genAlt (AltDef _ genSingle pr) = (view $ re pr) <$> genSingle
  IsoSchema base iso -> view iso <$> base

instance ToGen s => ToGen (Schema ann s) where
  toGen schema = (cataNT genAlg) (hforget schema)
