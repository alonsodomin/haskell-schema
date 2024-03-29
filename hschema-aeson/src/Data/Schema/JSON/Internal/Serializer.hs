{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Schema.JSON.Internal.Serializer where

import           Control.Applicative.Free
import           Control.Functor.HigherOrder
import           Control.Lens                hiding (iso)
import           Control.Monad.State         (State)
import qualified Control.Monad.State         as ST
import           Control.Natural
import qualified Data.Aeson.Types            as JSON
import           Data.Functor.Contravariant
import           Data.Functor.Sum
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NEL
import           Data.Maybe
import           Data.Schema.Internal.Types
import           Data.Text                   (Text)

newtype JsonSerializer a = JsonSerializer { runJsonSerializer :: a -> JSON.Value }

instance Contravariant JsonSerializer where
  contramap f (JsonSerializer g) = JsonSerializer $ g . f

newtype JsonDeserializer a = JsonDeserializer { runJsonDeserializer :: JSON.Value -> JSON.Parser a }

instance Functor JsonDeserializer where
  fmap f (JsonDeserializer g) = JsonDeserializer $ \x -> fmap f (g x)

instance Applicative JsonDeserializer where
  pure x = JsonDeserializer $ \_ -> pure x
  (JsonDeserializer l) <*> (JsonDeserializer r) = JsonDeserializer $ \x -> l x <*> r x

class ToJsonSerializer s where
  toJsonSerializer :: s ~> JsonSerializer

class ToJsonDeserializer s where
  toJsonDeserializer :: s ~> JsonDeserializer

instance (ToJsonSerializer p, ToJsonSerializer q) => ToJsonSerializer (Sum p q) where
  toJsonSerializer (InL l) = toJsonSerializer l
  toJsonSerializer (InR r) = toJsonSerializer r

toJsonSerializerAlg :: ToJsonSerializer p => HAlgebra (SchemaF p) JsonSerializer
toJsonSerializerAlg = wrapNT $ \case
  PrimitiveSchema p -> toJsonSerializer p

  RecordSchema fields -> JsonSerializer $ \obj -> JSON.Object $ ST.execState (runAp (encodeFieldOf obj) (unwrapField fields)) Map.empty
    where encodeFieldOf :: o -> FieldDef o JsonSerializer v -> State (HashMap Text JSON.Value) v
          encodeFieldOf o (RequiredField name (JsonSerializer serialize) getter) = do
            let el = view getter o
            ST.modify $ Map.insert name (serialize el)
            return el
          encodeFieldOf o (OptionalField name (JsonSerializer serialize) getter) = do
            let el = view getter o
            ST.modify $ Map.insert name (maybe JSON.Null serialize el)
            return el

  UnionSchema alts -> JsonSerializer $ \value -> head . catMaybes . NEL.toList $ fmap (encodeAlt value) alts
    where singleAttrObj :: Text -> JSON.Value -> JSON.Value
          singleAttrObj n v = JSON.Object $ Map.insert n v Map.empty

          encodeAlt :: o -> AltDef JsonSerializer o -> Maybe JSON.Value
          encodeAlt o (AltDef name (JsonSerializer serialize) pr) = do
            json <- serialize <$> o ^? pr
            return $ singleAttrObj name json

  AliasSchema (JsonSerializer base) iso -> JsonSerializer $ \value -> base (view (re iso) value)

instance ToJsonSerializer p => ToJsonSerializer (Schema p) where
  toJsonSerializer schema = cataNT toJsonSerializerAlg (unwrapSchema schema)

instance (ToJsonDeserializer p, ToJsonDeserializer q) => ToJsonDeserializer (Sum p q) where
  toJsonDeserializer (InL l) = toJsonDeserializer l
  toJsonDeserializer (InR r) = toJsonDeserializer r

toJsonDeserializerAlg :: ToJsonDeserializer p => HAlgebra (SchemaF p) JsonDeserializer
toJsonDeserializerAlg = wrapNT $ \case
  PrimitiveSchema p -> toJsonDeserializer p

  RecordSchema fields -> JsonDeserializer $ \case
    JSON.Object obj -> runAp decodeField $ unwrapField fields
      where decodeField :: FieldDef o JsonDeserializer v -> JSON.Parser v
            decodeField (RequiredField name (JsonDeserializer deserial) _) = JSON.explicitParseField deserial obj name
            decodeField (OptionalField name (JsonDeserializer deserial) _) = JSON.explicitParseFieldMaybe deserial obj name
    other -> fail $ "Expected JSON Object but got: " ++ show other

  UnionSchema alts -> JsonDeserializer $ \case
    JSON.Object obj -> head . catMaybes . NEL.toList $ fmap lookupParser alts
      where lookupParser :: AltDef JsonDeserializer a -> Maybe (JSON.Parser a)
            lookupParser (AltDef name (JsonDeserializer deserial) pr) = do
              altParser <- deserial <$> Map.lookup name obj
              return $ view (re pr) <$> altParser
    other ->  fail $ "Expected JSON Object but got: " ++ show other

  AliasSchema (JsonDeserializer base) iso -> JsonDeserializer (fmap (view iso) . base)

instance ToJsonDeserializer p => ToJsonDeserializer (Schema p) where
  toJsonDeserializer schema = cataNT toJsonDeserializerAlg (unwrapSchema schema)
