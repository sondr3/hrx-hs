module HRX.ParserSpec (spec) where

import HRX.Internal (isPathChar, pPath)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

spec :: Spec
spec = parallel $ do
  describe "path-character" $ do
    it "parses correct chars" $
      all ((== True) . isPathChar) ['a', 'b', '.', '@', '$'] `shouldBe` True
    it "does not allow illegal chars" $
      all ((== False) . isPathChar) ['/', ':', '\\', '\DEL', '\FF'] `shouldBe` True

  describe "parses path" $ do
    it "parses" $ do
      parse pPath "" "dir/file1" `shouldParse` "dir/file1"
      parse pPath "" "path/to/file2" `shouldParse` "path/to/file2"
    it "fails" $ do
      parse pPath "" `shouldFailOn` "/file"
      parse pPath "" `shouldFailOn` "dir//file"
