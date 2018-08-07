{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Schema.Model where

import           Control.Lens
import           Data.Aeson
import qualified Data.Schema             as S
import           Data.Schema.JSON
import           Data.Schema.JSON.Simple (JsonField, JsonSchema)
import qualified Data.Schema.JSON.Simple as JSON
import           Data.Text               (Text)
import           Data.Vector             (Vector)
import           Test.QuickCheck
import           Test.Schema.QuickCheck

data Role =
    UserRole UserRole
  | AdminRole AdminRole
  deriving (Eq, Show)

data UserRole = UserRole'
  deriving (Eq, Show)

data AdminRole = AdminRole' { department :: String, subordinateCount :: Int }
  deriving (Eq, Show)

_UserRole :: Prism' Role UserRole
_UserRole = prism' UserRole $ \case
    UserRole x -> Just x
    _          -> Nothing

_AdminRole :: Prism' Role AdminRole
_AdminRole = prism' AdminRole $ \case
    AdminRole x -> Just x
    _           -> Nothing

departmentProp :: JsonField AdminRole String
departmentProp = S.field "department" JSON.string (to department)

subordinateCountProp :: JsonField AdminRole Int
subordinateCountProp = S.field "subordinateCount" JSON.int (to subordinateCount)

roleSchema :: JsonSchema Role
roleSchema = S.oneOf'
           [ S.alt "user" (S.const' UserRole') _UserRole
           , S.alt "admin" (S.record' (AdminRole' <$> departmentProp <*> subordinateCountProp)) _AdminRole
           ]

data Person = Person { personName :: Text, birthDate :: Int, roles :: Vector Role }
  deriving (Eq, Show)

personSchema :: JsonSchema Person
personSchema = S.record'
             ( Person
             <$> S.field "name" JSON.text (to personName)
             <*> S.field "birthDate" JSON.int (to birthDate)
             <*> S.field "roles" (S.seq' roleSchema) (to roles)
             )

instance ToJSON Person where
  toJSON = runJsonSerializer . toJsonSerializer $ personSchema

instance FromJSON Person where
  parseJSON = runJsonDeserializer . toJsonDeserializer $ personSchema

instance Arbitrary Person where
  arbitrary = toGen personSchema
