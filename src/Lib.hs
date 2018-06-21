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
import Data.Schema (Schema, Prop)
import qualified Data.Schema as S
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock
import           Data.Vector     (Vector)

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
departmentProp = S.prop "department" S.StringSchema (to department)

subordinateCountProp :: Prop AdminRole Int
subordinateCountProp = S.prop "subordinateCount" S.IntSchema (to subordinateCount)

roleSchema :: Schema Role
roleSchema = S.union
           [ S.alt "user" (S.const UserRole') _UserRole
           , S.alt "admin" (S.record (AdminRole' <$> departmentProp <*> subordinateCountProp)) _AdminRole
           ]

data Person = Person { personName :: Text, birthDate :: Int, roles :: Vector Role }

personSchema :: Schema Person
personSchema = S.record
             ( Person
             <$> S.prop "name" S.StringSchema (to personName)
             <*> S.prop "birthDate" S.IntSchema (to birthDate)
             <*> S.prop "roles" (S.seq roleSchema) (to roles)
             )
