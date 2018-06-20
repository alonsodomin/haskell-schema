{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}

module Data.Schema.Types where

import           Control.Applicative.Free
import           Control.Lens
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Vector              (Vector)

data PropDef o a = PropDef
  { propName     :: Text
  , propSchema   :: Schema a
  , propAccessor :: Getter o a
  }

type Prop o a = Ap (PropDef o) a
type Props o = Prop o o

prop :: Text -> Schema a -> Getter o a -> Prop o a
prop name schema getter = liftAp (PropDef name schema getter)

-- emptyProps :: forall a. Props' a ()
-- emptyProps = Pure ()

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
  NoSchema :: Schema ()
  ListSchema :: Schema a -> Schema (Vector a)
  RecordSchema :: Props a -> Schema a
  UnionSchema :: [AltDef a] -> Schema a

-- voidGetter :: forall a. Getter a ()
-- voidGetter = to (const ())

-- emptyRecord :: forall a. Schema a
-- emptyRecord =
--   let voidPropDef :: PropDef a ()
--       voidPropDef = PropDef T.empty NoSchema voidGetter

--       lifted :: Props a
--       lifted = liftAp voidPropDef
--   in RecordSchema lifted

type Serializer a b   = Schema a -> (a -> b)
type Deserializer a b = Schema b -> (a -> Either String b)
