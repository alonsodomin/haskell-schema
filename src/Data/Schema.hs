module Data.Schema
     ( module Data.Schema.JSON
     , Prop
     , Props
     , prop
     , alt
     , Schema (IntSchema, StringSchema, BoolSchema)
     , const
     , record
     , seq
     , union
     ) where

import           Data.Schema.JSON
import           Data.Schema.Types
import           Prelude           hiding (const, seq)
