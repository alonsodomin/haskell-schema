module Test.Schema.Utils where

import           System.Directory
import           System.IO

getTestFolder :: IO FilePath
getTestFolder = do
  baseDir <- getCurrentDirectory
  return $ baseDir ++ "/test/"

testFilePath :: String -> IO String
testFilePath f = do
  testDir <- getTestFolder
  return $ testDir ++ f

loadTestFile :: String -> IO String
loadTestFile f = testFilePath f >>= readFile
