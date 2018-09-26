{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes          #-}

module Data.Schema.JSON.Simple where

import           Control.Functor.HigherOrder
import           Control.Lens
import           Data.HashMap.Strict             (HashMap)
import           Data.Schema
import           Data.Schema.Internal.Types
import           Data.Schema.JSON.Internal.Types
import           Data.Scientific
import           Data.Text                       (Text)
import qualified Data.Text                       as T

-- | Simple JSON schema type
type JsonSchema a = Schema' (HMutu JsonPrimitive Schema') a

-- | Simple JSON field type
type JsonField o a = Field (Schema' (HMutu JsonPrimitive Schema')) o a

-- | Define a text primitive
text :: JsonSchema Text
text = prim' $ HMutu JsonText

-- | Define a string primitive
string :: JsonSchema String
string = alias' text (iso T.unpack T.pack)

-- | Define a scientific number primitive
number :: JsonSchema Scientific
number = prim' $ HMutu JsonNumber

-- | Define an integral primitive
int :: Integral a => JsonSchema a
int = alias' number $ iso (\x -> either truncate id $ floatingOrInteger x) fromIntegral

-- | Define a floating point primitive
real :: RealFloat a => JsonSchema a
real = alias' number $ iso (\x -> either id fromIntegral $ floatingOrInteger x) fromFloatDigits

hash :: JsonSchema a -> JsonSchema (HashMap Text a)
hash base = prim' $ HMutu (JsonMap base)
