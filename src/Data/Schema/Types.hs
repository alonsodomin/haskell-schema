{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Schema.Types where

import Control.Applicative.Free
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)

data PropDef o a = PropDef
  { propName :: Text
  , propSchema :: Schema a
  , propAccessor :: Getter o a
  }

type Props o = Ap (PropDef o) o

-- propDef :: Text -> Schema a -> Getter o a -> Props o
-- propDef name schema get = liftAp $ ((PropDef name schema get) :: PropDef o a)

-- required :: PropDef o a -> Props o
-- required = liftAp

--type Props a = Props' a a

-- emptyProps :: forall a. Props' a ()
-- emptyProps = Pure ()

data Alt a = forall b. Alt
  { altId :: Text
  , altSchema :: Schema b
  , altPrism :: Prism' a b
  }

data Schema a where
  IntSchema :: Schema Int
  BoolSchema :: Schema Bool
  StringSchema :: Schema Text
  NoSchema :: Schema ()
  ListSchema :: Schema a -> Schema (Vector a)
  RecordSchema :: Props o -> Schema o
  UnionSchema :: [Alt a] -> Schema a

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