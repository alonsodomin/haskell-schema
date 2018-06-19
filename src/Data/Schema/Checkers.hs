{-# LANGUAGE TypeOperators #-}

module Data.Schema.Checkers where

import           Control.Natural
import           Data.Schema.Types
import           Test.QuickCheck

class ToGen s where
  toGen :: s ~> Gen
