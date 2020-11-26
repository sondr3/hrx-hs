module HRX.ExamplesSpec (spec, listExamples) where

import Control.Monad (forM_)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import HRX.Internal (pArchive)
import HRX.TestUtils (testParse)
import System.Directory (getCurrentDirectory, listDirectory)
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Megaparsec
import Text.Printf (printf)

listExamples :: String -> IO [FilePath]
listExamples dir = do
  curr <- getCurrentDirectory
  filter (\x -> "hrx" `isSuffixOf` x) <$> listDirectory (curr <> dir)

spec :: Spec
spec = do
  curr <- runIO getCurrentDirectory

  describe "valid examples" $ do
    files <- runIO $ listExamples "/spec/example"
    let dir = curr <> "/spec/example/"
    forM_ files $ \file ->
      it (printf "should parse %s" file) $ do
        content <- readFile (dir <> file)
        testParse pArchive `shouldSucceedOn` T.pack content

  describe "invalid examples" $ do
    files <- runIO $ listExamples "/spec/example/invalid"
    let dir = curr <> "/spec/example/invalid/"
    forM_ files $ \file ->
      it (printf "should not parse %s" file) $ do
        content <- readFile (dir <> file)
        testParse pArchive `shouldFailOn` T.pack content
