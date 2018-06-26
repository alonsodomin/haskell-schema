{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Schema.QuickCheck where

import           Control.Applicative.Free
import           Control.Lens
import           Control.Natural
import Control.Functor.HigherOrder
import           Data.Functor.Sum
import           Data.Schema.Types
import qualified Data.Vector              as Vector
import           Test.QuickCheck

class ToGen a where
  toGen :: a ~> Gen

instance (ToGen p, ToGen q) => ToGen (Sum p q) where
  toGen (InL l) = toGen l
  toGen (InR r) = toGen r

instance ToGen p => ToGen (Schema p) where
  toGen (HFix (PrimitiveSchema p)) = toGen p
  toGen (HFix (SeqSchema elemSchema)) = Vector.fromList <$> listOf (toGen elemSchema)
  toGen (HFix (RecordSchema props)) = runAp genProp props
    where genProp :: ToGen p => PropDef p o v -> Gen v
          genProp (PropDef _ sch _) = toGen sch
  toGen (HFix (UnionSchema alts)) = oneof $ fmap genAlt alts
    where genAlt :: ToGen p => AltDef p a -> Gen a
          genAlt (AltDef _ sch pr) = (view $ re pr) <$> toGen sch
