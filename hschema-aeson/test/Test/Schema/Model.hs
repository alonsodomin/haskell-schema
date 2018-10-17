{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Test.Schema.Model where

import           Control.Lens
import           Data.Aeson
import           Data.Schema             (HasSchema (..))
import qualified Data.Schema             as S
import           Data.Schema.JSON
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
adminRole = S.record
          ( AdminRole'
          <$> S.field "department"       JSON.string (to department)
          <*> S.field "subordinateCount" JSON.int    (to subordinateCount)
          )

roleSchema :: JsonSchema Role
roleSchema = S.oneOf
           [ S.alt "user"  (S.const UserRole') _UserRole
           , S.alt "admin" adminRole           _AdminRole
           ]

data Person = Person { personName :: String, birthDate :: Maybe Int, roles :: [Role] }
  deriving (Eq, Show)

personSchema :: JsonSchema Person
personSchema = S.record
             ( Person
             <$> S.field    "name"      JSON.string                      (to personName)
             <*> S.optional "birthDate" JSON.int                         (to birthDate)
             <*> S.field    "roles"     (S.list $ JSON.array roleSchema) (to roles)
             )

instance HasSchema Person where
  type PrimitivesOf Person = JsonType

  getSchema = personSchema
