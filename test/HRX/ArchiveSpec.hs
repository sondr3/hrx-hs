module HRX.ArchiveSpec (spec) where

import Data.Maybe (fromJust)
import HRX.Internal
import HRX.TestUtils (liftEither, testParse)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  let archive = liftEither $ testParse pArchive "<===> file\nfile contents\n<===> dir/\n<===>\ncomment contents\n<===> super/sub\nsub contents\n<===> very/deeply/\n<===> very/deeply/nested/file\nnested contents\n<===> last\nthe last file"

  describe "findEntry" $ do
    it "doesn't return an empty path" $ findEntry "" archive `shouldBe` Nothing
    it "doesn't return a path that's not in the archive" $ findEntry "non/existent/file" archive `shouldBe` Nothing
    it "doesn't return an implicit directory" $ findEntry "super" archive `shouldBe` Nothing
    it "doesn't return a file wih a slash" $ findEntry "super/sub/" archive `shouldBe` Nothing
    it "returns a file at the root level" $ do
      let entry = fromJust $ findEntry "file" archive
      entryContent (entryData entry) `shouldBe` Just "file contents\n"

    it "returns a file in a directory" $ do
      let entry = fromJust $ findEntry "super/sub" archive
      entryContent (entryData entry) `shouldBe` Just "sub contents\n"

    it "returns an explicit directory" $ do
      let entry = fromJust $ findEntry "dir" archive
      entryPath entry `shouldBe` "dir/"

    it "returns an explicit directory with a leading slash" $ do
      let entry = fromJust $ findEntry "dir/" archive
      entryPath entry `shouldBe` "dir/"

  describe "readEntry" $ do
    it "returns Nothing for an empty path" $ readEntry "" archive `shouldBe` Nothing
    it "returns Nothing for a path that's not in the archive" $ readEntry "non/existent/file" archive `shouldBe` Nothing
    it "returns Nothing for an implicit directory" $ readEntry "super" archive `shouldBe` Nothing
    it "returns Nothing for a file wih a slash" $ readEntry "super/sub/" archive `shouldBe` Nothing
    it "returns Nothing for a directory" $ readEntry "dir" archive `shouldBe` Nothing
    it "returns the contents of a file at the root level" $ readEntry "file" archive `shouldBe` Just "file contents\n"
    it "returns the contents of a file in a directory" $ readEntry "super/sub" archive `shouldBe` Just "sub contents\n"

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
