{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative.Free
import Control.Lens
import Data.Schema
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock

stringIso :: Iso' String Text
stringIso = iso T.pack T.unpack

data Role =
    UserRole UserRole
  | AdminRole AdminRole
  deriving (Eq, Show)

data UserRole = UserRole' { userName :: Text }
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

adminDept :: Getter AdminRole Text
adminDept = to department

subordinateCountGetter :: Getter AdminRole Int
subordinateCountGetter = to subordinateCount

userNameProp = liftAp $ PropDef "name" StringSchema (to userName)
userRoleAlt = Alt "user" (RecordSchema (UserRole' <$> userNameProp)) _UserRole

departmentProp = liftAp $ PropDef "department" StringSchema adminDept
subordinateCountProp = liftAp $ PropDef "subordinateCount" IntSchema subordinateCountGetter
adminRoleAlt = Alt "admin" (RecordSchema (AdminRole' <$> departmentProp <*> subordinateCountProp)) _AdminRole

userSchema :: Schema Role
userSchema = UnionSchema [userRoleAlt, adminRoleAlt]

data Person1 = Person1 { aName :: String, aAge :: Int }

data Person = Person { name :: String, birthDate :: UTCTime, roles :: [Role] }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
