{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

module Data.Schema.Types where

import           Control.Applicative.Free
import           Control.Lens
import           Data.Text                (Text)
import           Data.Vector              (Vector)
import           Prelude                  hiding (const, seq)
import Data.Time.Clock
import Data.Time.Clock.POSIX

data PropDef o a = PropDef
  { propName     :: Text
  , propSchema   :: Schema a
  , propAccessor :: Getter o a
  }

type Prop o a = Ap (PropDef o) a
type Props o = Prop o o

utcTimeIso :: Iso' UTCTime Rational
utcTimeIso = iso (toRational . utcTimeToPOSIXSeconds) (posixSecondsToUTCTime . fromRational)

prop :: Text -> Schema a -> Getter o a -> Prop o a
prop name schema getter = liftAp (PropDef name schema getter)

data AltDef a = forall b. AltDef
  { altName   :: Text
  , altSchema :: Schema b
  , altPrism  :: Prism' a b
  }

alt :: Text -> Schema b -> Prism' a b -> AltDef a
alt = AltDef

data Schema a where
  IntSchema :: Schema Int
  BoolSchema :: Schema Bool
  StringSchema :: Schema Text
  SeqSchema :: Schema a -> Schema (Vector a)
  RecordSchema :: Props a -> Schema a
  UnionSchema :: [AltDef a] -> Schema a

const :: a -> Schema a
const a = RecordSchema $ Pure a

record :: Props a -> Schema a
record = RecordSchema

seq :: Schema a -> Schema (Vector a)
seq = SeqSchema

union :: [AltDef a] -> Schema a
union = UnionSchema

type Serializer a b   = Schema a -> (a -> b)
type Deserializer a b = Schema b -> (a -> Either String b)
