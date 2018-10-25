module Test.Schema.Utils where

import           System.Directory
import           System.IO

getTestFolder :: IO FilePath
getTestFolder = do
  baseDir <- getCurrentDirectory
  return $ baseDir ++ "/test/"

loadTestFile :: String -> IO String
loadTestFile f = do
  testDir <- getTestFolder
  readFile $ testDir ++ f
