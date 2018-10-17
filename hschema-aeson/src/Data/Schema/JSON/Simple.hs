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

-- | Define a text primitive
text :: JsonSchema Text
text = prim $ HMutu JsonText

-- | Define a string primitive
string :: JsonSchema String
string = alias (iso T.unpack T.pack) text

-- | Define a scientific number primitive
number :: JsonSchema Scientific
number = prim $ HMutu JsonNumber

-- | Define an integral primitive
int :: Integral a => JsonSchema a
int = alias (iso (\x -> either truncate id $ floatingOrInteger x) fromIntegral) number

-- | Define a floating point primitive
real :: RealFloat a => JsonSchema a
real = alias (iso (\x -> either id fromIntegral $ floatingOrInteger x) fromFloatDigits) number

hash :: JsonSchema a -> JsonSchema (HashMap Text a)
hash base = prim $ HMutu (JsonMap base)
