module Test.Schema.JSON
     ( verifyJsonSchema
     ) where

import           Data.Aeson
import           Data.Aeson.Text
import           Data.Schema.JSON
import qualified Data.Text.Lazy    as T
import           Test.Hspec
import           Test.QuickCheck
import           Test.Schema.Model
import           Test.Schema.Utils

samplePerson :: Person
samplePerson = Person "foo" (Just 12) [
    mkUserRole
  , mkAdminRole "bar" 4
  ]

prop_reverse :: Person -> Bool
prop_reverse person = decode (encode person) == (Just person)

describeJsonSerialization :: IO ()
describeJsonSerialization = hspec $ do
  describe "toJsonSerializer" $ do
    it "should generate valid JSON" $ do
      expectedJSON <- loadTestFile "expected-model.json"
      (T.unpack $ encodeToLazyText samplePerson) `shouldBe` expectedJSON

verifyJsonSchema :: IO ()
verifyJsonSchema = do
  quickCheck prop_reverse
  describeJsonSerialization
