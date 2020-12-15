module HRX.ExamplesSpec (spec, listExamples) where

import Control.Monad (forM_)
import Data.List (isSuffixOf)
import qualified Data.Text.IO as TIO
import HRX.Internal (pArchive, toHRX)
import HRX.TestUtils (parseFile, testParse)
import System.Directory (getCurrentDirectory, listDirectory)
import Test.Hspec (Spec, describe, it, runIO, shouldBe, xit)
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
        content <- TIO.readFile (dir <> file)
        testParse pArchive `shouldSucceedOn` content

  describe "valid examples output match" $ do
    -- Ignore a single file due to newlines between boundaries not making it a 1-to-1 deserialization
    input <- runIO $ filter (/= "directory.hrx") <$> listExamples "/spec/example"
    let dir = curr <> "/spec/example/"
    forM_ input $ \file ->
      it (printf "%s matches output" file) $ do
        content <- TIO.readFile (dir <> file)
        archive <- parseFile content
        toHRX archive `shouldBe` content

  describe "directory contents" $ do
    it "cannot have it" $ testParse pArchive `shouldFailOn` "<===> dir/\nA directory can't have text contents.\n"

  describe "invalid examples" $ do
    describe "duplicates" $ do
      it "duplicate files" $ testParse pArchive `shouldFailOn` "<======> file\n<======> file\n"
      it "duplicate dirs" $ testParse pArchive `shouldFailOn` "<======> dir/\n<======> dir/\n"
      xit "file as parent" $
        -- TODO: File name collision difference to reference implementation
        testParse pArchive `shouldFailOn` "<======> file\n<======> file/sub\n"

  describe "invalid boundaries" $ do
    it "must begin with a boundary" $ testParse pArchive `shouldFailOn` "A HRX file must begin with a boundary."
    it "empty" $ testParse pArchive `shouldFailOn` "<>"
    it "unopened" $ testParse pArchive `shouldFailOn` "======>"
    it "unclosec" $ testParse pArchive `shouldFailOn` "<======"

  describe "invalid paths" $ do
    it "initial slash" $ testParse pArchive `shouldFailOn` "<======> /file\n"
    it "double slash" $ testParse pArchive `shouldFailOn` "<======> dir//file\n"
    it "final slash" $ testParse pArchive `shouldFailOn` "<======> dir//\n"
    it "single dot" $ testParse pArchive `shouldFailOn` "<======> .\n"
    it "double dot" $ testParse pArchive `shouldFailOn` "<======> ..\n"
    it "single dot component" $ testParse pArchive `shouldFailOn` "<======> dir/./file\n"
    it "double dot component" $ testParse pArchive `shouldFailOn` "<======> dir/../file\n"
    it "backslash" $ testParse pArchive `shouldFailOn` "<======> dir\file\n"
    it "invalid ascii" $ testParse pArchive `shouldFailOn` "<======> \DEL\n"
    it "colon" $ testParse pArchive `shouldFailOn` "<======> C:/file\n"
    it "no space before path" $ testParse pArchive `shouldFailOn` "<======>file\n"

  it "multi comment" $ testParse pArchive `shouldFailOn` "<===>\nA comment can't be followed by another comment.\n<===>"
