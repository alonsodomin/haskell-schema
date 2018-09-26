{-# LANGUAGE RankNTypes #-}

module Data.Schema
     ( Field
     , Fields
     , field
     , optional
     , alt
     , Schema
     , Schema'
     , HasSchema (..)
     , annotate
     , deannotate
     , prim
     , prim'
     , const
     , const'
     , record
     , record'
     , seq
     , seq'
     , list
     , list'
     , oneOf
     , oneOf'
     , alias
     , alias'
     ) where

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

-- | Define an alternative
alt :: Text -> s b -> Prism' a b -> AltDef s a
alt = AltDef

-- | Define an annotated schema for primitives of type `p`
prim :: ann -> p a -> Schema ann p a
prim ann primAlg = Schema (hcofree ann $ PrimitiveSchema primAlg)

prim' :: p a -> Schema' p a
prim' p = deannotate (prim () p)

-- | Define a schema for a type that is always constant
const :: ann -> a -> Schema ann p a
const ann a = Schema (hcofree ann (RecordSchema $ pure a))

const' :: a -> Schema' p a
const' a = deannotate (const () a)

-- | Define the schema of record using the given fields
record :: ann -> Fields (Schema ann p) a -> Schema ann p a
record ann ps = Schema (hcofree ann (RecordSchema $ hoistField unwrapSchema ps))

record' :: Fields (Schema' p) a -> Schema' p a
record' fs = deannotate (record () $ hoistField (annotate ()) fs)

-- | Define the schema of a vector based on the element type
seq :: ann -> Schema ann p a -> Schema ann p (Vector a)
seq ann elemSchema = Schema (hcofree ann (SeqSchema $ unwrapSchema elemSchema))

seq' :: Schema' p a -> Schema' p (Vector a)
seq' base = deannotate (seq () $ annotate () base)

-- | Define the schema of a list based on the element type

listIso :: Iso' (Vector a) [a]
listIso = iso Vector.toList Vector.fromList

list :: ann -> Schema ann p a -> Schema ann p [a]
list ann elemSchema = alias ann (seq ann elemSchema) listIso

list' :: Schema' p a -> Schema' p [a]
list' base = alias' (seq' base) listIso

-- | Define the schema of an union (coproduct) type based on the given alternatives
oneOf :: ann -> [AltDef (Schema ann p) a] -> Schema ann p a
oneOf ann alts = Schema (hcofree ann (UnionSchema $ fmap (hfmap unwrapSchema) $ NEL.fromList alts))

oneOf' :: [AltDef (Schema' p) a] -> Schema' p a
oneOf' alts = deannotate $ oneOf () $ fmap (hfmap $ annotate ()) alts

-- | Define an schema alias that is isomorphic to another one using the given ISO transformation
alias :: ann -> Schema ann p a -> Iso' a b -> Schema ann p b
alias ann base i = Schema (hcofree ann (AliasSchema (unwrapSchema base) i))

alias' :: Schema' p a -> Iso' a b -> Schema' p b
alias' base f = deannotate $ alias () (annotate () base) f
