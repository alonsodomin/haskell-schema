{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Schema.Model where

import           Control.Lens
import           Data.Aeson
import qualified Data.Schema             as S
import           Data.Schema.JSON
import           Data.Schema.JSON.Simple (JsonSchema)
import qualified Data.Schema.JSON.Simple as JSON
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

adminRole :: JsonSchema AdminRole
adminRole = S.record'
          ( AdminRole'
          <$> S.field "department"       JSON.string (to department)
          <*> S.field "subordinateCount" JSON.int    (to subordinateCount)
          )

roleSchema :: JsonSchema Role
roleSchema = S.oneOf'
           [ S.alt "user"  (S.const' UserRole') _UserRole
           , S.alt "admin" adminRole            _AdminRole
           ]

data Person = Person { personName :: String, birthDate :: Int, roles :: [Role] }
  deriving (Eq, Show)

personSchema :: JsonSchema Person
personSchema = S.record'
             ( Person
             <$> S.field "name"      JSON.string          (to personName)
             <*> S.field "birthDate" JSON.int             (to birthDate)
             <*> S.field "roles"     (S.list' roleSchema) (to roles)
             )

instance ToJSON Person where
  toJSON = runJsonSerializer . toJsonSerializer $ personSchema

instance FromJSON Person where
  parseJSON = runJsonDeserializer . toJsonDeserializer $ personSchema

instance Arbitrary Person where
  arbitrary = toGen personSchema
