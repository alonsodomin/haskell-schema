{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Schema.JSON
     ( JsonProp
     , JsonSchema
     , JsonPrimitive(..)
     , prim
     , ToJsonSerializer(..)
     , ToJsonDeserializer(..)
     ) where

import           Control.Applicative.Free
import           Control.Lens
import           Control.Monad.State      (State)
import qualified Control.Monad.State      as ST
import           Control.Natural
import Control.Functor.HigherOrder
import           Data.Aeson               (parseJSON)
import qualified Data.Aeson               as Json
import qualified Data.Aeson.Types         as Json
import           Data.Functor.Sum
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as Map
import           Data.Maybe
import           Data.Schema.Types
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Test.QuickCheck.Gen      as QC
import           Test.Schema.QuickCheck

type JsonSerializer a = a -> Json.Value
type JsonDeserializer a = Json.Value -> Json.Parser a

class ToJsonSerializer s where
  toJsonSerializer :: s ~> JsonSerializer

class ToJsonDeserializer s where
  toJsonDeserializer :: s ~> JsonDeserializer

data JsonPrimitive a where
  JsonInt :: JsonPrimitive Int
  JsonString :: JsonPrimitive Text

type JsonSchema a = Schema JsonPrimitive a
type JsonProp o a = Prop (Schema JsonPrimitive) o a

prim :: Text -> JsonPrimitive a -> Getter o a -> JsonProp o a
prim name primSchema getter = prop name (HFix $ PrimitiveSchema primSchema) getter

instance ToJsonSerializer JsonPrimitive where
  toJsonSerializer JsonInt    = Json.Number . fromIntegral
  toJsonSerializer JsonString = Json.String

instance (ToJsonSerializer p, ToJsonSerializer q) => ToJsonSerializer (Sum p q) where
  toJsonSerializer (InL l) = toJsonSerializer l
  toJsonSerializer (InR r) = toJsonSerializer r

instance ToGen JsonPrimitive where
  toGen JsonInt    = QC.chooseAny
  toGen JsonString = T.pack <$> (QC.listOf QC.chooseAny)

instance ToJsonSerializer p => ToJsonSerializer (Schema p) where
  toJsonSerializer (HFix (PrimitiveSchema p)) = toJsonSerializer p

  toJsonSerializer (HFix (SeqSchema elemSchema)) = \a -> Json.Array $ fmap (toJsonSerializer elemSchema) a

  toJsonSerializer (HFix (RecordSchema props)) = \obj -> Json.Object $ ST.execState (runAp (encodePropOf obj) props) Map.empty
    where encodePropOf :: ToJsonSerializer p => o -> PropDef p o v -> State (HashMap Text Json.Value) v
          encodePropOf o (PropDef name schema getter) = do
            let el = view getter o
            ST.modify $ Map.insert name (toJsonSerializer schema $ el)
            return el

  toJsonSerializer (HFix (UnionSchema alts)) = \value -> head . catMaybes $ fmap (encodeAlt value) alts
    where objSingleAttr :: Text -> Json.Value -> Json.Value
          objSingleAttr n v = Json.Object $ Map.insert n v Map.empty

          encodeAlt :: ToJsonSerializer p => o -> AltDef p o -> Maybe Json.Value
          encodeAlt o (AltDef i schema pr) = (objSingleAttr i) <$> (toJsonSerializer schema) <$> (o ^? pr)

instance ToJsonDeserializer JsonPrimitive where
  toJsonDeserializer JsonInt    = parseJSON
  toJsonDeserializer JsonString = parseJSON

instance ToJsonDeserializer p => ToJsonDeserializer (Schema p) where
  toJsonDeserializer (HFix (PrimitiveSchema p)) = toJsonDeserializer p

  toJsonDeserializer (HFix (SeqSchema elemSchema)) = \json -> case json of
    Json.Array v -> traverse (toJsonDeserializer elemSchema) v
    other        -> fail $ "Expected a JSON array but got: " ++ (show other)

  toJsonDeserializer (HFix (RecordSchema props)) = \json -> case json of
    Json.Object obj -> runAp (decodePropFrom obj) props
      where decodePropFrom :: ToJsonDeserializer p => HashMap Text Json.Value -> PropDef p o v -> Json.Parser v
            decodePropFrom jsonObj (PropDef name schema _) = Json.explicitParseField (toJsonDeserializer schema) jsonObj name
    other -> fail $ "Expected JSON Object but got: " ++ (show other)

  toJsonDeserializer (HFix (UnionSchema alts)) = \json -> case json of
    Json.Object obj -> head . catMaybes $ fmap lookupParser alts
      where lookupParser :: ToJsonDeserializer p => AltDef p a -> Maybe (Json.Parser a)
            lookupParser (AltDef i schema pr) = do
              altParser <- (toJsonDeserializer schema) <$> Map.lookup i obj
              return $ (view $ re pr) <$> altParser
    other ->  fail $ "Expected JSON Object but got: " ++ (show other)
