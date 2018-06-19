{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Schema.JSON
     ( serializer
     , deserializer
     ) where

import Data.Aeson (parseJSON)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import Data.Schema.Types
import Control.Applicative.Free
import Control.Lens
import Control.Monad.State (State)
import qualified Control.Monad.State as ST
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)

serializer :: Serializer a Json.Value
serializer IntSchema = Json.Number . fromIntegral
serializer BoolSchema = Json.Bool
serializer StringSchema = Json.String
serializer (ListSchema s) = \a -> Json.Array $ fmap (serializer s) a
serializer (RecordSchema ps) = \value -> Json.Object $ ST.execState (runAp (step value) ps) Map.empty
  where step :: o -> PropDef o v -> State (HashMap Text Json.Value) v
        step obj (PropDef name schema getter) = do
            let el = view getter obj
            ST.modify $ Map.insert name (serializer schema $ el)
            return el
serializer (UnionSchema alts) = undefined

deserializer :: Schema a -> (Json.Value -> Json.Parser a)
deserializer IntSchema = parseJSON
deserializer BoolSchema = parseJSON
deserializer StringSchema = parseJSON
deserializer (ListSchema s) = \json -> case json of
  Json.Array v -> traverse (deserializer s) v
  other        -> fail $ "Expected a JSON array but got: " ++ (show other)
deserializer (RecordSchema ps) = \json -> case json of
  Json.Object obj -> runAp (step obj) ps
    where step :: HashMap Text Json.Value -> PropDef o v -> Json.Parser v
          step jsonObj (PropDef name schema _) =
            Json.explicitParseField (\v -> deserializer schema $ v) jsonObj name
  other -> fail $ "Expected JSON Object but got: " ++ (show other)
deserializer (UnionSchema alts) = undefined