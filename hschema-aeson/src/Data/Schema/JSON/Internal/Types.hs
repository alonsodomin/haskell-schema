{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}

module Data.Schema.JSON.Internal.Types where

import           Control.Applicative                  (liftA2)
import           Data.Aeson                           (parseJSON)
import qualified Data.Aeson.Types                     as JSON
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.Schema.JSON.Internal.Serializer
import           Data.Schema.PrettyPrint
import           Data.Scientific
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Text.Prettyprint.Doc            ((<+>))
import qualified Data.Text.Prettyprint.Doc            as PP
import qualified Test.QuickCheck                      as QC
import qualified Test.QuickCheck.Gen                  as QC
import           Test.QuickCheck.Instances.Scientific ()
import           Test.Schema.QuickCheck.Internal.Gen

data JsonPrimitive (f :: (* -> *)) a where
  JsonNumber :: JsonPrimitive f Scientific
  JsonText   :: JsonPrimitive f Text
  JsonBool   :: JsonPrimitive f Bool
  JsonMap    :: f a -> JsonPrimitive f (HashMap Text a)

-- instance Show (f a) => Show (JsonPrimitive f a) where
--   show JsonNumber      = "JSON Number"
--   show JsonText        = "JSON Text"
--   show JsonBool        = "JSON Bool"
--   show (JsonMap value) = "JSON Map (Text -> " ++ (show value) ++ ")"

instance ToJsonSerializer f => ToJsonSerializer (JsonPrimitive f) where
  toJsonSerializer JsonNumber      = JsonSerializer $ JSON.Number
  toJsonSerializer JsonText        = JsonSerializer $ JSON.String
  toJsonSerializer JsonBool        = JsonSerializer $ JSON.Bool
  toJsonSerializer (JsonMap value) = JsonSerializer $ \x ->
    JSON.Object $ Map.map (runJsonSerializer . toJsonSerializer $ value) x

instance ToJsonDeserializer f => ToJsonDeserializer (JsonPrimitive f) where
  toJsonDeserializer JsonNumber = JsonDeserializer $ parseJSON
  toJsonDeserializer JsonText   = JsonDeserializer $ parseJSON
  toJsonDeserializer JsonBool   = JsonDeserializer $ parseJSON
  toJsonDeserializer (JsonMap value) = JsonDeserializer $ \case
    JSON.Object obj -> Map.foldrWithKey Map.insert Map.empty <$> traverse (runJsonDeserializer . toJsonDeserializer $ value) obj

instance ToGen f => ToGen (JsonPrimitive f) where
  toGen JsonNumber      = QC.arbitrary
  toGen JsonText        = T.pack <$> (QC.listOf QC.chooseAny)
  toGen JsonBool        = QC.arbitrary :: (QC.Gen Bool)
  toGen (JsonMap value) = Map.fromList <$> (QC.listOf $ liftA2 ((,)) (T.pack <$> (QC.listOf QC.chooseAny)) (toGen value))

instance ToSchemaDoc f => ToSchemaDoc (JsonPrimitive f) where
  toSchemaDoc JsonNumber      = SchemaDoc $ PP.pretty "Number"
  toSchemaDoc JsonText        = SchemaDoc $ PP.pretty "Text"
  toSchemaDoc JsonBool        = SchemaDoc $ PP.pretty "Bool"
  toSchemaDoc (JsonMap value) = SchemaDoc $ PP.pretty "Map { Text ->" <+> (getDoc . toSchemaDoc $ value) <+> PP.pretty "}"

instance ToSchemaLayout f => ToSchemaLayout (JsonPrimitive f) where
  toSchemaLayout JsonNumber = SchemaLayout $ PP.unsafeViaShow
  toSchemaLayout JsonText   = SchemaLayout $ PP.unsafeViaShow
  toSchemaLayout JsonBool   = SchemaLayout $ PP.unsafeViaShow
  toSchemaLayout (JsonMap value) = SchemaLayout $ \x ->
    PP.vsep $ fmap (\(k,v) -> PP.pretty k <+> PP.pretty "->" <+> runSchemaLayout (toSchemaLayout value) v) $ Map.toList x
