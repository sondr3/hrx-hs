module HRX.HRXSpec (spec) where

import qualified Data.Text as T
import HRX.Internal (Archive (..), Entry (..))
import HRX.TestUtils (testParser)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "parse" $ do
    it "parses an empty file" $ null (archiveEntries $ testParser "")
    it "converts the file to UTF-8" $
      null (archiveEntries $ testParser "<===> いか\n") `shouldBe` False -- Haskell does not use UT8 by default
    it "requires the string to be UTF-8" $
      null (archiveEntries $ testParser "<===> \xc3\x28\n") `shouldBe` False -- Haskell does not use UT8 by default
    it "parses contents without a newline" $
      entryContent (head (archiveEntries $ testParser "<===> file\ncontents")) `shouldBe` "contents"

    describe "with a single file" $ do
      let subject = testParser (T.unlines ["<===> file", "contents"])

      it "parses one entry" $ length (archiveEntries subject) `shouldBe` 1
      it "parses the filename" $ entryFile (head (archiveEntries subject)) `shouldBe` "file"
      it "parses the contents" $ entryContent (head (archiveEntries subject)) `shouldBe` "contents\n"

    describe "parses contents with boundary-like sequences" $ do
      let subject = testParser (T.unlines ["<===> file", "<==>", "inline <===>", "<====>"])

      it "parses the contents" $
        entryContent (head $ archiveEntries subject) `shouldBe` T.unlines ["<==>", "inline <===>", "<====>"]

    describe "with a comment" $ do
      let subject = testParser (T.unlines ["<===>", "comment", "<===> file", "contents"])

      it "parses one entry" $ length (archiveEntries subject) `shouldBe` 1
      it "parses the filename" $ entryFile (head (archiveEntries subject)) `shouldBe` "file"
      it "parses the contents" $ entryContent (head (archiveEntries subject)) `shouldBe` "contents\n"
      it "parses the comment" $ entryComment (head (archiveEntries subject)) `shouldBe` Just "comment"

    describe "with multiple files" $ do
      let subject = testParser (T.unlines ["<===> file 1", "contents 1", "<===> file 2", "contents 2"])

      it "parses two entries" $ length (archiveEntries subject) `shouldBe` 2
      it "parses the first filename" $ entryFile (head $ archiveEntries subject) `shouldBe` "file 1"
      it "parses the first contents" $ entryContent (head $ archiveEntries subject) `shouldBe` "contents 1\n"
      it "parses the second filename" $ entryFile (archiveEntries subject !! 1) `shouldBe` "file 2"
      it "parses the second contents" $ entryContent (archiveEntries subject !! 1) `shouldBe` "contents 2\n"
