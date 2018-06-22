{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( personSchema
    , roleSchema
    ) where

import           Control.Lens
import           Data.Schema  (JsonProp, JsonSchema)
import qualified Data.Schema  as S
import           Data.Text    (Text)
import           Data.Vector  (Vector)

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

departmentProp :: JsonProp AdminRole Text
departmentProp = S.prim "department" S.JsonString (to department)

subordinateCountProp :: JsonProp AdminRole Int
subordinateCountProp = S.prim "subordinateCount" S.JsonInt (to subordinateCount)

roleSchema :: JsonSchema Role
roleSchema = S.union
           [ S.alt "user" (S.const UserRole') _UserRole
           , S.alt "admin" (S.record (AdminRole' <$> departmentProp <*> subordinateCountProp)) _AdminRole
           ]

data Person = Person { personName :: Text, birthDate :: Int, roles :: Vector Role }
  deriving (Eq, Show)

personSchema :: JsonSchema Person
personSchema = S.record
             ( Person
             <$> S.prim "name" S.JsonString (to personName)
             <*> S.prim "birthDate" S.JsonInt (to birthDate)
             <*> S.prop "roles" (S.seq roleSchema) (to roles)
             )
