{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Schema.JSON
     ( JsonSerializer(..)
     , JsonDeserializer(..)
     , ToJsonSerializer(..)
     , ToJsonDeserializer(..)
     , JsonPrimitive(..)
     , JsonSchema
     , JsonSchema'
     , text
     , text'
     , string
     , string'
     , int
     , int'
     , JsonField
     , JsonField'
     , jsonField
     , jsonField'
     ) where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                hiding (iso)
import qualified Control.Lens                as Lens
import           Control.Monad.State         (State)
import qualified Control.Monad.State         as ST
import           Control.Natural
import           Data.Aeson                  (parseJSON)
import qualified Data.Aeson                  as Json
import qualified Data.Aeson.Types            as Json
import           Data.Functor.Sum
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.Maybe
import           Data.Schema.Types
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Test.QuickCheck.Gen         as QC
import           Test.Schema.QuickCheck

newtype JsonSerializer a   = JsonSerializer { runJsonSerializer :: a -> Json.Value }
newtype JsonDeserializer a = JsonDeserializer { runJsonDeserializer :: Json.Value -> Json.Parser a }

class ToJsonSerializer s where
  toJsonSerializer :: s ~> JsonSerializer

class ToJsonDeserializer s where
  toJsonDeserializer :: s ~> JsonDeserializer

data JsonPrimitive a where
  JsonInt  :: JsonPrimitive Int
  JsonText :: JsonPrimitive Text

instance Show (JsonPrimitive a) where
  show JsonInt  = "JSON Int"
  show JsonText = "JSON Text"

type JsonSchema ann a = Schema ann JsonPrimitive a
type JsonSchema' a = JsonSchema () a

text :: ann -> JsonSchema ann Text
text ann = prim ann JsonText

text' :: JsonSchema' Text
text' = text ()

string :: ann -> JsonSchema ann String
string ann = iso ann (text ann) (Lens.iso T.unpack T.pack)

string' :: JsonSchema' String
string' = string ()

int :: ann -> JsonSchema ann Int
int ann = prim ann JsonInt

int' :: JsonSchema' Int
int' = int ()

type JsonField ann o a = Field (Schema ann JsonPrimitive) o a
type JsonField' o a = JsonField () o a

jsonField :: ann -> Text -> JsonPrimitive a -> Getter o a -> JsonField ann o a
jsonField ann name alg getter = field name (prim ann alg) getter

jsonField' :: Text -> JsonPrimitive a -> Getter o a -> JsonField' o a
jsonField' = jsonField ()

instance ToJsonSerializer JsonPrimitive where
  toJsonSerializer JsonInt  = JsonSerializer $ Json.Number . fromIntegral
  toJsonSerializer JsonText = JsonSerializer $ Json.String

instance (ToJsonSerializer p, ToJsonSerializer q) => ToJsonSerializer (Sum p q) where
  toJsonSerializer (InL l) = toJsonSerializer l
  toJsonSerializer (InR r) = toJsonSerializer r

instance ToGen JsonPrimitive where
  toGen JsonInt  = QC.chooseAny
  toGen JsonText = T.pack <$> (QC.listOf QC.chooseAny)

toJsonSerializerAlg :: ToJsonSerializer p => HAlgebra (SchemaF p) JsonSerializer
toJsonSerializerAlg = wrapNT $ \case
  PrimitiveSchema p -> toJsonSerializer p

  SeqSchema serializer -> JsonSerializer $ \vec -> Json.Array $ fmap (runJsonSerializer serializer) vec

  RecordSchema fields -> JsonSerializer $ \obj -> Json.Object $ ST.execState (runAp (encodeFieldOf obj) fields) Map.empty
    where encodeFieldOf :: o -> FieldDef o JsonSerializer v -> State (HashMap Text Json.Value) v
          encodeFieldOf o (FieldDef name (JsonSerializer serialize) getter) = do
            let el = view getter o
            ST.modify $ Map.insert name (serialize el)
            return el

  UnionSchema alts -> JsonSerializer $ \value -> head . catMaybes $ fmap (encodeAlt value) alts
    where singleAttrObj :: Text -> Json.Value -> Json.Value
          singleAttrObj n v = Json.Object $ Map.insert n v Map.empty

          encodeAlt :: o -> AltDef JsonSerializer o -> Maybe Json.Value
          encodeAlt o (AltDef name (JsonSerializer serialize) pr) = do
            json <- serialize <$> o ^? pr
            return $ singleAttrObj name json

  IsoSchema (JsonSerializer base) iso -> JsonSerializer $ \value -> base (view (re iso) value)

instance ToJsonSerializer p => ToJsonSerializer (Schema ann p) where
  toJsonSerializer schema = (cataNT toJsonSerializerAlg) (hforget schema)

instance ToJsonDeserializer JsonPrimitive where
  toJsonDeserializer JsonInt  = JsonDeserializer $ parseJSON
  toJsonDeserializer JsonText = JsonDeserializer $ parseJSON

toJsonDeserializerAlg :: ToJsonDeserializer p => HAlgebra (SchemaF p) JsonDeserializer
toJsonDeserializerAlg = wrapNT $ \case
  PrimitiveSchema p -> toJsonDeserializer p

  SeqSchema elemSchema -> JsonDeserializer $ \json -> case json of
    Json.Array v -> traverse (runJsonDeserializer elemSchema) v
    other        -> fail $ "Expected a JSON array but got: " ++ (show other)

  RecordSchema fields -> JsonDeserializer $ \json -> case json of
    Json.Object obj -> runAp decodeField fields
      where decodeField :: FieldDef o JsonDeserializer v -> Json.Parser v
            decodeField (FieldDef name (JsonDeserializer deserial) _) = Json.explicitParseField deserial obj name
    other -> fail $ "Expected JSON Object but got: " ++ (show other)

  UnionSchema alts -> JsonDeserializer $ \json -> case json of
    Json.Object obj -> head . catMaybes $ fmap lookupParser alts
      where lookupParser :: AltDef JsonDeserializer a -> Maybe (Json.Parser a)
            lookupParser (AltDef name (JsonDeserializer deserial) pr) = do
              altParser <- deserial <$> Map.lookup name obj
              return $ (view $ re pr) <$> altParser
    other ->  fail $ "Expected JSON Object but got: " ++ (show other)
    
  IsoSchema (JsonDeserializer base) iso -> JsonDeserializer $ \json -> (view iso) <$> (base json)

instance ToJsonDeserializer p => ToJsonDeserializer (Schema ann p) where
  toJsonDeserializer schema = (cataNT toJsonDeserializerAlg) (hforget schema)
