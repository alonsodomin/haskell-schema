{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Schema.QuickCheck.Internal.Gen
     ( ToGen (..)
     ) where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens
import           Control.Natural
import Control.Monad (liftM)
import Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Functor.Sum
import qualified Data.List.NonEmpty          as NEL
import           Data.Schema.Internal.Types
import qualified Data.Vector                 as Vector
import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

class ToGen a where
  toGen :: MonadTrans m => a ~> m Gen

instance (ToGen p, ToGen q) => ToGen (Sum p q) where
  toGen (InL l) = toGen l
  toGen (InR r) = toGen r

optional :: Gen a -> Gen (Maybe a)
optional base = Gen.frequency [(1, return Nothing), (3, liftM Just base)]

genAlg :: (MonadTrans m, ToGen p) => HAlgebra (SchemaF p) (m Gen)
genAlg = wrapNT $ \case
  PrimitiveSchema p    -> toGen p
  SeqSchema elemSchema -> Vector.fromList <$> Gen.listOf <$> elemSchema
  RecordSchema fields  -> runAp fieldGen fields
    where fieldGen :: MonadTrans m => FieldDef o (m Gen) ~> (m Gen)
          fieldGen = \case
            RequiredField _ base _ -> base
            OptionalField _ base _ -> optional <$> base
  UnionSchema alts     -> Gen.oneof . NEL.toList $ fmap genAlt alts
    where genAlt :: AltDef Gen a -> Gen a
          genAlt (AltDef _ genSingle pr) = (view $ re pr) <$> genSingle
  AliasSchema base iso -> view iso <$> base

instance ToGen s => ToGen (Schema ann s) where
  toGen schema = (cataNT genAlg) (hforget schema)
