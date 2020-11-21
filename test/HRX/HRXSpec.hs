module HRX.HRXSpec (spec) where

import qualified Data.Text as T
import HRX (Archive (..), Entry (..))
import HRX.TestUtils (testParser)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "parse" $ do
    it "parses an empty file" $
      null (archiveEntries $ testParser "")
    it "converts the file to UTF-8" True -- Haskell does not use UT8 by default
    it "requires the string to be UTF-8" True -- Haskell does not use UT8 by default
    context "with a single file" $ do
      let subject = testParser (T.unlines ["<===> file", "contents"])

      it "parses one entry" $
        length (archiveEntries subject) `shouldBe` 1
      it "parses the filename" $
        entryFile (head (archiveEntries subject)) `shouldBe` "file"
      it "parses the contents" $
        entryContent (head (archiveEntries subject)) `shouldBe` "contents\n"

    it "parses contents with boundary-like sequences" $ do
      let subject = testParser (T.unlines ["<===> file", "<==>", "inline <===>", "<====>"])
      entryContent (head $ archiveEntries subject) `shouldBe` T.unlines ["<==>", "inline <===>", "<====>"]
