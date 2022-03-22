{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}

module Data.Schema.JSON.Internal.Types where

import           Control.Applicative                  (liftA2)
import           Control.Functor.HigherOrder
import           Data.Aeson                           (parseJSON)
import qualified Data.Aeson.Types                     as JSON
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.Schema.Internal.Types
import           Data.Schema.JSON.Internal.Serializer
import           Data.Schema.PrettyPrint
import           Data.Scientific
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as Vector
import           Prettyprinter                        ((<+>))
import qualified Prettyprinter                        as PP
import qualified Test.QuickCheck                      as QC
import qualified Test.QuickCheck.Gen                  as QC
import           Test.QuickCheck.Instances.Scientific ()
import           Test.Schema.QuickCheck.Internal.Gen

data JsonPrimitive (f :: (* -> *)) (a :: *) where
  JsonNumber :: JsonPrimitive f Scientific
  JsonText   :: JsonPrimitive f Text
  JsonBool   :: JsonPrimitive f Bool
  JsonArray  :: f a -> JsonPrimitive f (Vector a)
  JsonMap    :: f a -> JsonPrimitive f (HashMap Text a)

type JsonType = HMutu JsonPrimitive Schema

-- | Simple JSON schema type
type JsonSchema = Schema JsonType

-- | Simple JSON field type
type JsonField o a = Field JsonSchema o a

instance ToJsonSerializer JsonType where
  toJsonSerializer jType = JsonSerializer $ case unmutu jType of
    JsonNumber      -> JSON.Number
    JsonText        -> JSON.String
    JsonBool        -> JSON.Bool
    JsonArray value -> JSON.Array . fmap (runJsonSerializer . toJsonSerializer $ value)
    JsonMap value   -> JSON.Object . Map.map (runJsonSerializer . toJsonSerializer $ value)

instance ToJsonDeserializer JsonType where
  toJsonDeserializer jType = JsonDeserializer $ case unmutu jType of
    JsonNumber      -> parseJSON
    JsonText        -> parseJSON
    JsonBool        -> parseJSON
    JsonArray value -> \case
      JSON.Array arr -> traverse (runJsonDeserializer . toJsonDeserializer $ value) arr
      other          -> fail $ "Expected a JSON array but got: " ++ show other
    JsonMap value   -> \case
      JSON.Object obj -> Map.foldrWithKey Map.insert Map.empty <$> traverse (runJsonDeserializer . toJsonDeserializer $ value) obj
      other           -> fail $ "Expected a JSON object but got: " ++ show other

instance ToGen JsonType where
  toGen jType = case unmutu jType of
    JsonNumber      -> QC.arbitrary
    JsonText        -> T.pack <$> QC.listOf QC.chooseAny
    JsonBool        -> QC.arbitrary :: (QC.Gen Bool)
    JsonArray value -> Vector.fromList <$> QC.listOf (toGen value)
    JsonMap value   -> Map.fromList <$> QC.listOf (liftA2 (,) (T.pack <$> QC.listOf QC.chooseAny) (toGen value))

instance ToSchemaDoc JsonType where
  toSchemaDoc settings jType = SchemaDoc $ case unmutu jType of
    JsonNumber      -> PP.pretty "Number"
    JsonText        -> PP.pretty "Text"
    JsonBool        -> PP.pretty "Bool"
    JsonArray value -> PP.pretty "[" <> (getDoc . toSchemaDoc settings $ value) <> PP.pretty "]"
    JsonMap value   -> PP.pretty "Map { Text ->" <+> (getDoc . toSchemaDoc settings $ value) <+> PP.pretty "}"

instance ToSchemaLayout JsonType where
  toSchemaLayout settings jType = SchemaLayout $ case unmutu jType of
    JsonNumber      -> PP.unsafeViaShow
    JsonText        -> PP.unsafeViaShow
    JsonBool        -> PP.unsafeViaShow
    JsonArray value -> PP.vsep . fmap (renderSchemaLayout (toSchemaLayout settings value)) . Vector.toList
    JsonMap value   -> PP.vsep . fmap (\(k,v) -> PP.pretty k <+> PP.pretty "->" <+> renderSchemaLayout (toSchemaLayout settings value) v) . Map.toList
