module HRX.ArchiveSpec (spec) where

import HRX.Internal
import HRX.TestUtils (liftEither, testParse)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "glob" $ do
    let archive = liftEither $ testParse pArchive "<===> file\nfile contents\n\n<===> dir/\n<===>\ncomment contents\n\n<===> super/sub\nsub contents\n\n<===> very/deeply/\n<===> very/deeply/nested/file\nnested contents\n\n<===> last\nthe last file"

  describe "findEntriesGlob" $ do
    it "returns nothing for an empty glob" $ findEntriesGlob "" archive `shouldBe` []
    it "returns nothing for a path that's not in the archive" $ findEntriesGlob "non/existent/file" archive `shouldBe` []
    it "doesn't return implicit directories" $ findEntriesGlob "super" archive `shouldBe` []
    it "doesn't return a file with a slash" $ findEntriesGlob "super/sub/" archive `shouldBe` []
    it "doesn't return an explicit directory without a leading slash" $ findEntriesGlob "dir" archive `shouldBe` []

    it "returns a file at the root level" $ do
      let entries = findEntriesGlob "file" archive

      length entries `shouldBe` 1
      entryPath (head entries) `shouldBe` "file"

    it "returns a file in a directory" $ do
      let entries = findEntriesGlob "super/sub" archive

      length entries `shouldBe` 1
      entryPath (head entries) `shouldBe` "super/sub"

    it "returns an explicit directory" $ do
      let entries = findEntriesGlob "dir/" archive

      length entries `shouldBe` 1
      entryPath (head entries) `shouldBe` "dir/"

    it "returns all matching files at the root level" $ do
      let entries = findEntriesGlob "*" archive

      length entries `shouldBe` 2
      entryPath (head entries) `shouldBe` "file"
      entryPath (head $ tail entries) `shouldBe` "last"

    it "returns all matching files in a directory" $ do
      let entries = findEntriesGlob "super/*" archive

      length entries `shouldBe` 1
      entryPath (head entries) `shouldBe` "super/sub"

    it "returns all matching entries recursively in a directory" $ do
      let entries = findEntriesGlob "very/**/*" archive

      length entries `shouldBe` 2
      entryPath (head entries) `shouldBe` "very/deeply/"
      entryPath (head $ tail entries) `shouldBe` "very/deeply/nested/file"
