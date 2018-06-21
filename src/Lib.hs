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
import           Data.Schema
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock
import           Data.Vector     (Vector)
import           Prelude         hiding (const, seq)

stringIso :: Iso' String Text
stringIso = iso T.pack T.unpack

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

departmentProp :: Prop AdminRole Text
departmentProp = prop "department" StringSchema (to department)

subordinateCountProp :: Prop AdminRole Int
subordinateCountProp = prop "subordinateCount" IntSchema (to subordinateCount)

roleSchema :: Schema Role
roleSchema = union
           [ alt "user" (const UserRole') _UserRole
           , alt "admin" (RecordSchema (AdminRole' <$> departmentProp <*> subordinateCountProp)) _AdminRole
           ]

data Person = Person { personName :: Text, birthDate :: Int, roles :: Vector Role }

personSchema :: Schema Person
personSchema = record
             ( Person
             <$> prop "name" StringSchema (to personName)
             <*> prop "birthDate" IntSchema (to birthDate)
             <*> prop "roles" (seq roleSchema) (to roles)
             )
