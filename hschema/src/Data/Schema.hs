{-# LANGUAGE RankNTypes #-}

module Data.Schema
     ( Field
     , Fields
     , field
     , alt
     , Schema
     , Schema'
     , HasSchema (..)
     , prim
     , prim'
     , const
     , const'
     , record
     , record'
     , opt
     , opt'
     , seq
     , seq'
     , list
     , list'
     , hash
     , hash'
     , oneOf
     , oneOf'
     , alias
     , alias'
     ) where

import           Control.Applicative.Free    (Ap (Pure), liftAp)
import           Control.Functor.HigherOrder
import           Control.Lens
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.List.NonEmpty          as NEL
import           Data.Schema.Internal.Types
import           Data.Text                   (Text)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as Vector
import           Prelude                     hiding (const, seq)

-- | Define a field
field :: Text -> s a -> Getter o a -> Field s o a
field name schema getter = liftAp (FieldDef name schema getter)

-- | Define an alternative
alt :: Text -> s b -> Prism' a b -> AltDef s a
alt = AltDef

-- | Define an annotated schema for primitives of type `p`
prim :: ann -> p a -> Schema ann p a
prim ann primAlg = hcofree ann $ PrimitiveSchema primAlg

prim' :: p a -> Schema' p a
prim' = prim ()

opt :: ann -> Schema ann p a -> Schema ann p (Maybe a)
opt ann base = hcofree ann (OptSchema base)

opt' :: Schema' p a -> Schema' p (Maybe a)
opt' = opt ()

-- | Define a schema for a type that is always constant
const :: ann -> a -> Schema ann p a
const ann a = hcofree ann (RecordSchema $ Pure a)

const' :: a -> Schema' p a
const' = const ()

-- | Define the schema of record using the given fields
record :: ann -> Fields (Schema ann p) a -> Schema ann p a
record ann ps = hcofree ann (RecordSchema ps)

record' :: Fields (Schema' p) a -> Schema' p a
record' = record ()

-- | Define the schema of a vector based on the element type
seq :: ann -> Schema ann p a -> Schema ann p (Vector a)
seq ann elemSchema = hcofree ann (SeqSchema elemSchema)

seq' :: Schema' p a -> Schema' p (Vector a)
seq' = seq ()

-- | Define the schema of a list based on the element type
list :: ann -> Schema ann p a -> Schema ann p [a]
list ann elemSchema = alias ann (seq ann elemSchema) (iso Vector.toList Vector.fromList)

list' :: Schema' p a -> Schema' p [a]
list' = list ()

hash :: Hashable k => ann -> Schema ann p k -> Schema ann p a -> Schema ann p (HashMap k a)
hash ann keySchema elemSchema = hcofree ann (HashSchema keySchema elemSchema)

hash' :: Hashable k => Schema' p k -> Schema' p a -> Schema' p (HashMap k a)
hash' = hash ()

-- | Define the schema of an union (coproduct) type based on the given alternatives
oneOf :: ann -> [AltDef (Schema ann p) a] -> Schema ann p a
oneOf ann alts = hcofree ann (UnionSchema $ NEL.fromList alts)

oneOf' :: [AltDef (Schema' p) a] -> Schema' p a
oneOf' = oneOf ()

-- | Define an schema alias that is isomorphic to another one using the given ISO transformation
alias :: ann -> Schema ann p a -> Iso' a b -> Schema ann p b
alias ann base i = hcofree ann (AliasSchema base i)

alias' :: Schema' p a -> Iso' a b -> Schema' p b
alias' = alias ()
