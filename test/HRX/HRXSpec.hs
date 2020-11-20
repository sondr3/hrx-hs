module HRX.HRXSpec (spec) where

import HRX (Archive (..))
import HRX.TestUtils (testParser, testParser')
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "parse" $ do
    it "parses an empty file" $
      null (archiveEntries $ testParser "")
    it "requires the string to be UTF-8" $
      testParser' "<===> \xc3\x28\n" `shouldBe` Left "Could not parse input"
