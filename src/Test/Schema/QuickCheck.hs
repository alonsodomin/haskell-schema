{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Test.Schema.QuickCheck where

import           Control.Applicative.Free
import           Control.Lens
import           Control.Natural
import           Data.Schema.Types
import qualified Data.Vector              as Vector
import           Test.QuickCheck

class ToGen a where
  toGen :: a ~> Gen

instance ToGen p => ToGen (Schema p) where
  toGen (PrimitiveSchema p) = toGen p
  toGen (SeqSchema elemSchema) = Vector.fromList <$> listOf (toGen elemSchema)
  toGen (RecordSchema props) = runAp genProp props
    where genProp :: ToGen p => PropDef p o v -> Gen v
          genProp (PropDef _ sch _) = toGen sch
  toGen (UnionSchema alts) = oneof $ fmap genAlt alts
    where genAlt :: ToGen p => AltDef p a -> Gen a
          genAlt (AltDef _ sch pr) = (view $ re pr) <$> toGen sch
