{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Schema.JSON
     ( JsonSerializer(..)
     , JsonDeserializer(..)
     , ToJsonSerializer(..)
     , ToJsonDeserializer(..)
     , JsonPrimitive(..)
     ) where

import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON (toJSON))
import           Data.Schema
import           Data.Schema.JSON.Internal.Serializer
import           Data.Schema.JSON.Internal.Types

instance (HasSchema a, ToJsonSerializer (PrimitivesOf a)) => ToJSON a where
  toJSON = runJsonSerializer . toJsonSerializer $ getSchema

instance (HasSchema a, ToJsonDeserializer (PrimitivesOf a)) => FromJSON a where
  parseJSON = runJsonDeserializer . toJsonDeserializer $ getSchema
