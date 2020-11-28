module HRX.ParsersSpec (spec) where

import qualified Data.Text as T
import HRX.Internal (Path (Path), isPathChar, pPath)
import Test.Hspec (Spec, describe, it, parallel, shouldBe)
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
      parse pPath "" "dir/file1\n" `parseSatisfies` (== Path "dir/file1")
      parse pPath "" "path/to/file2\n" `parseSatisfies` (== Path "path/to/file2")
    it "fails" $ do
      parse pPath "" `shouldFailOn` "/file\n"
      parse pPath "" `shouldFailOn` "dir//file\n"
      parse pPath "" `shouldFailOn` "dir//\n"
      parse pPath "" `shouldFailOn` ".\n"
      parse pPath "" `shouldFailOn` "..\n"
      parse pPath "" `shouldFailOn` "dir/./file\n"
      parse pPath "" `shouldFailOn` "dir/../file\n"
      parse pPath "" `shouldFailOn` "dir\file\n"
      parse pPath "" `shouldFailOn` T.pack ['"', '"']
      parse pPath "" `shouldFailOn` "C:/file\n"