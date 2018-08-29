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

type JsonSchema a = Schema' JsonPrimitive a

text :: JsonSchema Text
text = prim' JsonText

string :: JsonSchema String
string = iso' text (Lens.iso T.unpack T.pack)

number :: JsonSchema Scientific
number = prim' JsonNumber

int :: Integral a => JsonSchema a
int = iso' number $ Lens.iso (\x -> either truncate id $ floatingOrInteger x) fromIntegral

real :: RealFloat a => JsonSchema a
real = iso' number $ Lens.iso (\x -> either id fromIntegral $ floatingOrInteger x) fromFloatDigits

type JsonField o a = Field (Schema' JsonPrimitive) o a

json :: Text -> JsonPrimitive a -> Getter o a -> JsonField o a
json name alg getter = field name (prim' alg) getter
