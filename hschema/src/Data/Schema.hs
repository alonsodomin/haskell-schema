{-# LANGUAGE RankNTypes #-}

module Data.Schema
     ( Field
     , Fields
     , field
     , optional
     , alt
     , Schema
     , HasSchema (..)
     , prim
     , const
     , record
     , asList
     , toList
     , oneOf
     , alias
     ) where

import           Control.Functor.HigherOrder
import           Control.Lens
import           Data.Functor.Invariant
import           Data.HashMap.Strict         (HashMap)
import           Data.Hashable               (Hashable)
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
prim :: p a -> Schema p a
prim primAlg = Schema (HFix $ PrimitiveSchema primAlg)

-- | Define a schema for a type that is always constant
const :: a -> Schema p a
const a = Schema (HFix (RecordSchema $ pure a))

-- | Define the schema of record using the given fields
record :: Fields (Schema p) a -> Schema p a
record ps = Schema (HFix (RecordSchema $ hoistField unwrapSchema ps))

-- | Define the schema of a list based on the element type
asList :: Iso' (Vector a) [a]
asList = iso Vector.toList Vector.fromList

toList :: Schema p (Vector a) -> Schema p [a]
toList = invmap Vector.toList Vector.fromList

-- | Define the schema of an union (coproduct) type based on the given alternatives
oneOf :: [AltDef (Schema p) a] -> Schema p a
oneOf alts = Schema (HFix (UnionSchema $ hfmap unwrapSchema <$> NEL.fromList alts))

-- | Define an schema alias that is isomorphic to another one using the given ISO transformation
alias :: Iso' a b -> Schema p a -> Schema p b
alias i = invmap (view i) (view . from $ i)
