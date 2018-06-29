{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Schema.Model where

import           Control.Lens
import           Data.Aeson
import           Data.Schema            (JsonProp, JsonSchema)
import qualified Data.Schema            as S
import           Data.Schema.JSON
import           Data.Text              (Text)
import           Data.Vector            (Vector)
import           Test.QuickCheck
import           Test.Schema.QuickCheck

data Role =
    UserRole UserRole
  | AdminRole AdminRole
  deriving (Eq, Show)

data UserRole = UserRole'
  deriving (Eq, Show)

data AdminRole = AdminRole' { department :: Text, subordinateCount :: Int }
  deriving (Eq, Show)

_UserRole :: Prism' Role UserRole
_UserRole = prism' UserRole $ \case
    UserRole x -> Just x
    _          -> Nothing

_AdminRole :: Prism' Role AdminRole
_AdminRole = prism' AdminRole $ \case
    AdminRole x -> Just x
    _           -> Nothing

departmentProp :: JsonProp_ AdminRole Text
departmentProp = S.prim_ "department" S.JsonString (to department)

subordinateCountProp :: JsonProp_ AdminRole Int
subordinateCountProp = S.prim_ "subordinateCount" S.JsonInt (to subordinateCount)

roleSchema :: JsonSchema_ Role
roleSchema = S.union_
           [ S.alt "user" (S.const_ UserRole') _UserRole
           , S.alt "admin" (S.record_ (AdminRole' <$> departmentProp <*> subordinateCountProp)) _AdminRole
           ]

data Person = Person { personName :: Text, birthDate :: Int, roles :: Vector Role }
  deriving (Eq, Show)

personSchema :: JsonSchema_ Person
personSchema = S.record_
             ( Person
             <$> S.prim_ "name" S.JsonString (to personName)
             <*> S.prim_ "birthDate" S.JsonInt (to birthDate)
             <*> S.prop "roles" (S.seq_ roleSchema) (to roles)
             )

instance ToJSON Person where
  toJSON = runJsonSerializer . toJsonSerializer $ personSchema

instance FromJSON Person where
  parseJSON = runJsonDeserializer . toJsonDeserializer $ personSchema

instance Arbitrary Person where
  arbitrary = toGen personSchema
