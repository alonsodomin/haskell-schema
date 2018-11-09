module Test.Schema.JSON
     ( verifyJsonSchema
     ) where

import           Data.Aeson
import           Data.Aeson.Text
import           Data.Convertible
import           Data.Schema.JSON
import qualified Data.Text.Lazy    as T
import           Test.Hspec
import           Test.QuickCheck
import           Test.Schema.Model
import           Test.Schema.Utils

samplePerson :: Person
samplePerson = Person "foo" (Just $ convert (12 :: Int)) [
    mkUserRole
  , mkAdminRole "bar" 4
  ]

samplePersonJSONFileName :: String
samplePersonJSONFileName = "expected-model.json"

samplePersonJSON :: IO String
samplePersonJSON = loadTestFile samplePersonJSONFileName

prop_reverse :: Person -> Bool
prop_reverse person = decode (encode person) == (Just person)

describeJsonSerialization :: IO ()
describeJsonSerialization = hspec $ do
  describe "toJsonSerializer" $ do
    it "should generate valid JSON" $ do
      expectedJSON <- samplePersonJSON
      (T.unpack $ encodeToLazyText samplePerson) `shouldBe` expectedJSON

    it "should parse the given JSON" $ do
      givenJSON     <- samplePersonJSON
      decodedPerson <- decodeFileStrict =<< testFilePath samplePersonJSONFileName
      decodedPerson `shouldBe` (Just samplePerson)

verifyJsonSchema :: IO ()
verifyJsonSchema = do
  quickCheck prop_reverse
  describeJsonSerialization
