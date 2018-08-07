{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Schema.JSON.Types where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                hiding (iso)
import           Control.Monad.State         (State)
import qualified Control.Monad.State         as ST
import           Control.Natural
import           Data.Aeson                  (parseJSON)
import qualified Data.Aeson.Types            as JSON
import           Data.Functor.Sum
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.Maybe
import           Data.Schema.Types
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Test.QuickCheck             as QC
import qualified Test.QuickCheck.Gen         as QC
import           Test.Schema.QuickCheck

newtype JsonSerializer a   = JsonSerializer { runJsonSerializer :: a -> JSON.Value }
newtype JsonDeserializer a = JsonDeserializer { runJsonDeserializer :: JSON.Value -> JSON.Parser a }

class ToJsonSerializer s where
  toJsonSerializer :: s ~> JsonSerializer

class ToJsonDeserializer s where
  toJsonDeserializer :: s ~> JsonDeserializer

data JsonPrimitive a where
  JsonInt  :: JsonPrimitive Int
  JsonText :: JsonPrimitive Text
  JsonBool :: JsonPrimitive Bool

instance Show (JsonPrimitive a) where
  show JsonInt  = "JSON Int"
  show JsonText = "JSON Text"
  show JsonBool = "JSON Bool"

instance ToJsonSerializer JsonPrimitive where
  toJsonSerializer JsonInt  = JsonSerializer $ JSON.Number . fromIntegral
  toJsonSerializer JsonText = JsonSerializer $ JSON.String
  toJsonSerializer JsonBool = JsonSerializer $ JSON.Bool

instance ToGen JsonPrimitive where
  toGen JsonInt  = QC.chooseAny
  toGen JsonText = T.pack <$> (QC.listOf QC.chooseAny)
  toGen JsonBool = QC.arbitrary :: (QC.Gen Bool)

instance (ToJsonSerializer p, ToJsonSerializer q) => ToJsonSerializer (Sum p q) where
  toJsonSerializer (InL l) = toJsonSerializer l
  toJsonSerializer (InR r) = toJsonSerializer r

toJsonSerializerAlg :: ToJsonSerializer p => HAlgebra (SchemaF p) JsonSerializer
toJsonSerializerAlg = wrapNT $ \case
  PrimitiveSchema p -> toJsonSerializer p

  SeqSchema serializer -> JsonSerializer $ \vec -> JSON.Array $ fmap (runJsonSerializer serializer) vec

  RecordSchema fields -> JsonSerializer $ \obj -> JSON.Object $ ST.execState (runAp (encodeFieldOf obj) fields) Map.empty
    where encodeFieldOf :: o -> FieldDef o JsonSerializer v -> State (HashMap Text JSON.Value) v
          encodeFieldOf o (FieldDef name (JsonSerializer serialize) getter) = do
            let el = view getter o
            ST.modify $ Map.insert name (serialize el)
            return el

  UnionSchema alts -> JsonSerializer $ \value -> head . catMaybes $ fmap (encodeAlt value) alts
    where singleAttrObj :: Text -> JSON.Value -> JSON.Value
          singleAttrObj n v = JSON.Object $ Map.insert n v Map.empty

          encodeAlt :: o -> AltDef JsonSerializer o -> Maybe JSON.Value
          encodeAlt o (AltDef name (JsonSerializer serialize) pr) = do
            json <- serialize <$> o ^? pr
            return $ singleAttrObj name json

  IsoSchema (JsonSerializer base) iso -> JsonSerializer $ \value -> base (view (re iso) value)

instance ToJsonSerializer p => ToJsonSerializer (Schema ann p) where
  toJsonSerializer schema = (cataNT toJsonSerializerAlg) (hforget schema)

instance (ToJsonDeserializer p, ToJsonDeserializer q) => ToJsonDeserializer (Sum p q) where
  toJsonDeserializer (InL l) = toJsonDeserializer l
  toJsonDeserializer (InR r) = toJsonDeserializer r

instance ToJsonDeserializer JsonPrimitive where
  toJsonDeserializer JsonInt  = JsonDeserializer $ parseJSON
  toJsonDeserializer JsonText = JsonDeserializer $ parseJSON
  toJsonDeserializer JsonBool = JsonDeserializer $ parseJSON

toJsonDeserializerAlg :: ToJsonDeserializer p => HAlgebra (SchemaF p) JsonDeserializer
toJsonDeserializerAlg = wrapNT $ \case
  PrimitiveSchema p -> toJsonDeserializer p

  SeqSchema elemSchema -> JsonDeserializer $ \json -> case json of
    JSON.Array v -> traverse (runJsonDeserializer elemSchema) v
    other        -> fail $ "Expected a JSON array but got: " ++ (show other)

  RecordSchema fields -> JsonDeserializer $ \json -> case json of
    JSON.Object obj -> runAp decodeField fields
      where decodeField :: FieldDef o JsonDeserializer v -> JSON.Parser v
            decodeField (FieldDef name (JsonDeserializer deserial) _) = JSON.explicitParseField deserial obj name
    other -> fail $ "Expected JSON Object but got: " ++ (show other)

  UnionSchema alts -> JsonDeserializer $ \json -> case json of
    JSON.Object obj -> head . catMaybes $ fmap lookupParser alts
      where lookupParser :: AltDef JsonDeserializer a -> Maybe (JSON.Parser a)
            lookupParser (AltDef name (JsonDeserializer deserial) pr) = do
              altParser <- deserial <$> Map.lookup name obj
              return $ (view $ re pr) <$> altParser
    other ->  fail $ "Expected JSON Object but got: " ++ (show other)

  IsoSchema (JsonDeserializer base) iso -> JsonDeserializer $ \json -> (view iso) <$> (base json)

instance ToJsonDeserializer p => ToJsonDeserializer (Schema ann p) where
  toJsonDeserializer schema = (cataNT toJsonDeserializerAlg) (hforget schema)
