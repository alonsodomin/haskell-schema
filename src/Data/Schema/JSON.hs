{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}

module Data.Schema.JSON
     ( serializer
     , deserializer
     ) where

import           Control.Applicative.Free
import           Control.Lens
import           Control.Monad.State      (State)
import qualified Control.Monad.State      as ST
import           Data.Aeson               (parseJSON)
import qualified Data.Aeson               as Json
import qualified Data.Aeson.Types         as Json
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.Maybe
import           Data.Schema.Types
import           Data.Text                (Text)

serializer :: Schema a -> (a -> Json.Value)
serializer IntSchema = Json.Number . fromIntegral
serializer BoolSchema = Json.Bool
serializer StringSchema = Json.String
serializer (SeqSchema s) = \a -> Json.Array $ fmap (serializer s) a
serializer (RecordSchema ps) = \value -> Json.Object $ ST.execState (runAp (step value) ps) Map.empty
  where step :: o -> PropDef o v -> State (HashMap Text Json.Value) v
        step obj (PropDef name schema getter) = do
            let el = view getter obj
            ST.modify $ Map.insert name (serializer schema $ el)
            return el
serializer (UnionSchema alts) = \value -> head . catMaybes $ fmap (decodeAlt value) alts
  where objSingleAttr :: Text -> Json.Value -> Json.Value
        objSingleAttr n v = Json.Object $ Map.insert n v Map.empty

        decodeAlt :: o -> AltDef o -> Maybe Json.Value
        decodeAlt obj (AltDef i schema pr) = (objSingleAttr i) <$> (serializer schema) <$> (obj ^? pr)

deserializer :: Schema a -> (Json.Value -> Json.Parser a)
deserializer IntSchema = parseJSON
deserializer BoolSchema = parseJSON
deserializer StringSchema = parseJSON
deserializer (SeqSchema s) = \json -> case json of
  Json.Array v -> traverse (deserializer s) v
  other        -> fail $ "Expected a JSON array but got: " ++ (show other)
deserializer (RecordSchema ps) = \json -> case json of
  Json.Object obj -> runAp (step obj) ps
    where step :: HashMap Text Json.Value -> PropDef o v -> Json.Parser v
          step jsonObj (PropDef name schema _) =
            Json.explicitParseField (\v -> deserializer schema $ v) jsonObj name
  other -> fail $ "Expected JSON Object but got: " ++ (show other)
deserializer (UnionSchema alts) = \json -> case json of
  Json.Object obj -> head . catMaybes $ fmap lookupParser alts
    where lookupParser :: AltDef a -> Maybe (Json.Parser a)
          lookupParser (AltDef i schema pr) = do
            altParser <- (deserializer schema) <$> Map.lookup i obj
            return $ (view $ re pr) <$> altParser
  other ->  fail $ "Expected JSON Object but got: " ++ (show other)
