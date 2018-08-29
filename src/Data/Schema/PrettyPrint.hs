module Data.Schema.PrettyPrint where

import           Data.Schema.Types
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Doc

type Doc = Doc.Doc Doc.AnsiStyle

newtype PP a = PP { getDoc :: Doc }

class PrettySchema s where
  pprintSchema :: s a -> Doc