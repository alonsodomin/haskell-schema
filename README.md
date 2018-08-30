# Haskell Schema

[![Build Status](https://travis-ci.org/alonsodomin/haskell-schema.svg?branch=master)](https://travis-ci.org/alonsodomin/haskell-schema)

This is a library inspired by (and an attempt to write a Haskell version of) [xenomorph](https://github.com/nuttycom/xenomorph),
  a Scala library for describing data.

## Motivation

The idea behind it is that, given a domain model you want to work with, you can use this library to build a description of it (or _schema_)
  that is totally independent of the actual code representation of the given domain model. After that, you can leverage the mechanics
  behind this library to generate QuickCheck generators, JSON parsers, binary codecs, etc.

### Isn't that much work? What about deriving `Generic`?

Deriving `Generic` from your data and deriving your encoders from there seems pretty reasonable, and it's usually very concise, isn't it?
  But there is a problem with that, usually the data that you are going to be serializing over the wire (that's why you need your JSON,
  binary, etc. codecs) forms part of your public protocol. That means that every time you modify one of those data items, you are in danger
  of breaking your compatibility.

On top of that, what about supporting two versions of your protocol? That will get hairy quite quickly. By defining the schema separated
  from the actual data types, you can evolve your domain model without modifying the actual schema, add a new schema version and even
  define migrations between them.

## How to use it?

Haskell Schema is distributed as a set of packages that together provide a cohesive set of features:

 * `hschema`: This is the core package, defining the base building pieces
 * `hschema-aeson`: This is a package that provides JSON encoding and decoding using Aeson.
 * `hschema-quickcheck`: This package will provide with QuickCheck generators based on our schema.
 * `hschema-prettyprinter`: This package brings pretty priting utilities.

In the following example we are going to make use of all those packages.

### Example

_**Note:** The following example has been extracted from the [xenomorph](https://github.com/nuttycom/xenomorph) library, which served as an inspiration for writing this one._

Let's start by defining a some data types alongside some lenses:

```haskell
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

import Control.Lens

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

data Person = Person { personName :: String, personAge :: Int, personRoles :: [Role] }
  deriving (Eq, Show)
```

Now, defining the schema for the `Person` data type, you define each of the fields individually (name, type and getter) and combine them using
  an applicative:

```haskell
import qualified Data.Schema             as S
import           Data.Schema.JSON
import           Data.Schema.JSON.Simple (JsonSchema)
import qualified Data.Schema.JSON.Simple as JSON

personSchema :: JsonSchema Person
personSchema = S.record'
             ( Person
             <$> S.field "name"  JSON.string          (to personName)
             <*> S.field "age"   JSON.int             (to personAge)
             <*> S.field "roles" (S.list' roleSchema) (to personRoles)
             )
```

The schema for the `Role` data type is defined as a list of alternatives alongside a prism as an accessor:

```haskell
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
```

Once you have defined the schema, by proving an instance for the `HasSchema` typeclass,
 you'll get JSON decoders, encoders, generators, etc. for free right away.

```haskell
import Data.Schema (HasSchema(..))

instance HasSchema Person where
  type Ann Person = ()
  type PrimitivesOf Person = JsonPrimitive

  getSchema = personSchema
```

### Pretty Printer

There is also built-in support for pretty printing schemas:

```haskell
import Data.Schema.PrettyPrint

putSchema personSchema
```

That will produce an output similar to the following:

```
* roles :: [
  - user
  - admin
    * subordinateCount :: Number
    * department :: Text
]
* birthDate :: Number
* name :: Text
```

Not happy with that? What about a pretty printer based on the given schema? Just use the `prettyPrinter` function, which will
return you a `a -> IO ()` function that you can use to print your data types:

```haskell
pprintPerson :: Person -> IO ()
pprintPerson = prettyPrinter personSchema
```