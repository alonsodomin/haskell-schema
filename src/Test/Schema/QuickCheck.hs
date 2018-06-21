{-# LANGUAGE GADTs #-}

module Test.Schema.QuickCheck where

import           Control.Applicative.Free
import           Control.Lens
import           Data.Schema.Types
import qualified Data.Text                as T
import qualified Data.Vector              as Vector
import           Test.QuickCheck.Gen

generator :: Schema a -> Gen a
generator IntSchema = chooseAny
generator BoolSchema = chooseAny
generator StringSchema = T.pack <$> (listOf chooseAny)
generator (SeqSchema elemSchema) = Vector.fromList <$> listOf (generator elemSchema)
generator (RecordSchema props) = runAp genProp props
  where genProp :: PropDef o v -> Gen v
        genProp (PropDef _ sch _) = generator sch
generator (UnionSchema alts) = oneof $ fmap genAlt alts
  where genAlt :: AltDef a -> Gen a
        genAlt (AltDef _ sch pr) = (view $ re pr) <$> generator sch
