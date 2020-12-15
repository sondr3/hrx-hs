module HRX.ArchiveSpec (spec) where

import HRX.Internal
import HRX.TestUtils (liftEither, testParse)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "glob" $ do
    let archive = liftEither $ testParse pArchive "<===> file\nfile contents\n\n<===> dir/\n<===>\ncomment contents\n\n<===> super/sub\nsub contents\n\n<===> very/deeply/\n<===> very/deeply/nested/file\nnested contents\n\n<===> last\nthe last file"

    it "returns nothing for an empty glob" $ entriesGlob "" archive `shouldBe` []
    it "returns nothing for a path that's not in the archive" $ entriesGlob "non/existent/file" archive `shouldBe` []
    it "doesn't return implicit directories" $ entriesGlob "super" archive `shouldBe` []
    it "doesn't return a file with a slash" $ entriesGlob "super/sub/" archive `shouldBe` []
    it "doesn't return an explicit directory without a leading slash" $ entriesGlob "dir" archive `shouldBe` []

    it "returns a file at the root level" $ do
      let entries = entriesGlob "file" archive

      length entries `shouldBe` 1
      entryPath (head entries) `shouldBe` "file"

    it "returns a file in a directory" $ do
      let entries = entriesGlob "super/sub" archive

      length entries `shouldBe` 1
      entryPath (head entries) `shouldBe` "super/sub"

    it "returns an explicit directory" $ do
      let entries = entriesGlob "dir/" archive

      length entries `shouldBe` 1
      entryPath (head entries) `shouldBe` "dir/"

    it "returns all matching files at the root level" $ do
      let entries = entriesGlob "*" archive

      length entries `shouldBe` 2
      entryPath (head entries) `shouldBe` "file"
      entryPath (head $ tail entries) `shouldBe` "last"

    it "returns all matching files in a directory" $ do
      let entries = entriesGlob "super/*" archive

      length entries `shouldBe` 1
      entryPath (head entries) `shouldBe` "super/sub"

    it "returns all matching entries recursively in a directory" $ do
      let entries = entriesGlob "very/**/*" archive

      length entries `shouldBe` 2
      entryPath (head entries) `shouldBe` "very/deeply/"
      entryPath (head $ tail entries) `shouldBe` "very/deeply/nested/file"
