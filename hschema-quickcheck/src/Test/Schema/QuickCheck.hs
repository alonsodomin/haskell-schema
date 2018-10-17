{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Schema.QuickCheck
     ( module Test.Schema.QuickCheck.Internal.Gen
     ) where

import           Data.Schema
import           Test.QuickCheck
import           Test.Schema.QuickCheck.Internal.Gen

instance (HasSchema a, ToGen (PrimitivesOf a)) => Arbitrary a where
  arbitrary = toGen getSchema
