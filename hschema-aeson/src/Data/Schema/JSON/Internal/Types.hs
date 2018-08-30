{-# LANGUAGE GADTs #-}

module Data.Schema.JSON.Internal.Types where

import           Data.Aeson                           (parseJSON)
import qualified Data.Aeson.Types                     as JSON
import           Data.Schema.JSON.Internal.Serializer
import           Data.Schema.PrettyPrint
import           Data.Scientific
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Prettyprint.Doc            as PP
import qualified Test.QuickCheck                      as QC
import qualified Test.QuickCheck.Gen                  as QC
import           Test.QuickCheck.Instances.Scientific ()
import           Test.Schema.QuickCheck

data JsonPrimitive a where
  JsonNumber :: JsonPrimitive Scientific
  JsonText   :: JsonPrimitive Text
  JsonBool   :: JsonPrimitive Bool

instance Show (JsonPrimitive a) where
  show JsonNumber = "JSON Number"
  show JsonText   = "JSON Text"
  show JsonBool   = "JSON Bool"

instance ToJsonSerializer JsonPrimitive where
  toJsonSerializer JsonNumber = JsonSerializer $ JSON.Number
  toJsonSerializer JsonText   = JsonSerializer $ JSON.String
  toJsonSerializer JsonBool   = JsonSerializer $ JSON.Bool

instance ToJsonDeserializer JsonPrimitive where
  toJsonDeserializer JsonNumber = JsonDeserializer $ parseJSON
  toJsonDeserializer JsonText   = JsonDeserializer $ parseJSON
  toJsonDeserializer JsonBool   = JsonDeserializer $ parseJSON

instance ToGen JsonPrimitive where
  toGen JsonNumber = QC.arbitrary
  toGen JsonText   = T.pack <$> (QC.listOf QC.chooseAny)
  toGen JsonBool   = QC.arbitrary :: (QC.Gen Bool)

instance ToSchemaDoc JsonPrimitive where
  toSchemaDoc JsonNumber = SchemaDoc $ PP.pretty "Number"
  toSchemaDoc JsonText   = SchemaDoc $ PP.pretty "Text"
  toSchemaDoc JsonBool   = SchemaDoc $ PP.pretty "Bool"

instance ToSchemaLayout JsonPrimitive where
  toSchemaLayout JsonNumber = SchemaLayout $ PP.unsafeViaShow
  toSchemaLayout JsonText   = SchemaLayout $ PP.unsafeViaShow
  toSchemaLayout JsonBool   = SchemaLayout $ PP.unsafeViaShow
