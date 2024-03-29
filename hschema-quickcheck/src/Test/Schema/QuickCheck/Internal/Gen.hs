{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Schema.QuickCheck.Internal.Gen
     ( ToGen (..)
     ) where

import           Control.Applicative         (liftA2)
import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens
import           Control.Monad               (liftM)
import           Control.Natural
import           Data.Functor.Sum
import qualified Data.HashMap.Strict         as Map
import qualified Data.List.NonEmpty          as NEL
import           Data.Schema.Internal.Types
import qualified Data.Vector                 as Vector
import           Test.QuickCheck             (Gen)
import qualified Test.QuickCheck             as Gen

optGen :: Gen a -> Gen (Maybe a)
optGen base = Gen.frequency [(1, return Nothing), (3, fmap Just base)]

class ToGen a where
  toGen :: a ~> Gen

instance (ToGen p, ToGen q) => ToGen (Sum p q) where
  toGen (InL l) = toGen l
  toGen (InR r) = toGen r

genAlg :: ToGen p => HAlgebra (SchemaF p) Gen
genAlg = wrapNT $ \case
  PrimitiveSchema p         -> toGen p
  RecordSchema (Field flds) -> runAp genField flds
    where genField :: FieldDef o Gen a -> Gen a
          genField (RequiredField _ g _) = g
          genField (OptionalField _ g _) = optGen g
  UnionSchema alts          -> Gen.oneof . NEL.toList $ fmap genAlt alts
    where genAlt :: AltDef Gen a -> Gen a
          genAlt (AltDef _ genSingle pr) = view (re pr) <$> genSingle
  AliasSchema base iso      -> view iso <$> base

instance ToGen s => ToGen (Schema s) where
  toGen schema = cataNT genAlg (unwrapSchema schema)
