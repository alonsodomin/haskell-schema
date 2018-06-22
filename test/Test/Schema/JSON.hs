module Test.Schema.JSON
     ( verifyJsonSchema
     ) where

import           Data.Aeson
import           Data.Schema.JSON
import           Test.QuickCheck
import           Test.Schema.Model

prop_reverse :: Person -> Bool
prop_reverse person = decode (encode person) == (Just person)

verifyJsonSchema :: IO ()
verifyJsonSchema = quickCheck prop_reverse
