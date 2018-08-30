{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Data.Schema.JSON.Simple where

import           Control.Lens           hiding (iso)
import qualified Control.Lens           as Lens
import           Data.Schema.JSON.Types
import           Data.Schema.Types
import           Data.Scientific
import           Data.Text              (Text)
import qualified Data.Text              as T

-- | Simple JSON schema type
type JsonSchema a = Schema' JsonPrimitive a

-- | Define a text primitive
text :: JsonSchema Text
text = prim' JsonText

-- | Define a string primitive
string :: JsonSchema String
string = iso' text (Lens.iso T.unpack T.pack)

-- | Define a scientific number primitive
number :: JsonSchema Scientific
number = prim' JsonNumber

-- | Define an integral primitive
int :: Integral a => JsonSchema a
int = iso' number $ Lens.iso (\x -> either truncate id $ floatingOrInteger x) fromIntegral

-- | Define a floating point primitive
real :: RealFloat a => JsonSchema a
real = iso' number $ Lens.iso (\x -> either id fromIntegral $ floatingOrInteger x) fromFloatDigits

-- | Simple JSON field type
type JsonField o a = Field (Schema' JsonPrimitive) o a

-- | Define a JSON field
json :: Text -> JsonPrimitive a -> Getter o a -> JsonField o a
json name alg getter = field name (prim' alg) getter
