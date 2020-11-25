{-# LANGUAGE OverloadedStrings #-}

module HRX.HRXSpec (spec) where

import qualified Data.Text as T
import HRX.Internal
import HRX.TestUtils (testParse)
import Test.Hspec
import Test.Hspec.Megaparsec

spec :: Spec
spec = parallel $ do
  describe "parse" $ do
    it "parses an empty file" $
      testParse pArchive "" `parseSatisfies` (null . archiveEntries)
    it "converts the file to UTF-8" $ -- Haskell does not use UT8 by default
      testParse pArchive "<===> いか\n" `parseSatisfies` ((== 1) . length . archiveEntries)
    it "requires the string to be UTF-8" $ -- Haskell does not use UT8 by default
      testParse pArchive "<===> \xc3\x28\n" `parseSatisfies` ((== 1) . length . archiveEntries)
    it "parses contents without a newline" $
      testParse pArchive "<===> file\ncontents" `parseSatisfies` ((== Just "contents") . entryContent . entryData . head . archiveEntries)

    describe "with a single file" $ do
      let t p = p pArchive (T.unlines ["<===> file", "contents"])

      it "parses one entry" $ t testParse `parseSatisfies` ((== 1) . length . archiveEntries)
      it "parses the filename" $ t testParse `parseSatisfies` ((== "file") . entryFile . entryData . head . archiveEntries)
      it "parses the contents" $ t testParse `parseSatisfies` ((== Just "contents\n") . entryContent . entryData . head . archiveEntries)

    describe "parses contents with boundary-like sequences" $ do
      let t p = p pArchive (T.unlines ["<===> file", "<==>", "inline <===>", "<====>"])

      it "parses the contents" $
        t testParse `parseSatisfies` ((== Just (T.unlines ["<==>", "inline <===>", "<====>"])) . entryContent . entryData . head . archiveEntries)

    describe "with a comment" $ do
      let t p = p pArchive (T.unlines ["<===>", "comment", "<===> file", "contents"])

      it "parses one entry" $ t testParse `parseSatisfies` ((== 1) . length . archiveEntries)
      it "parses the filename" $ t testParse `parseSatisfies` ((== "file") . entryFile . entryData . head . archiveEntries)
      it "parses the contents" $ t testParse `parseSatisfies` ((== Just "contents\n") . entryContent . entryData . head . archiveEntries)
      it "parses the comment" $ t testParse `parseSatisfies` ((== Just "comment\n") . entryComment . head . archiveEntries)

    describe "with multiple files" $ do
      let oT p = p pArchive (T.unlines ["<===> file 1", "contents 1\n", "<===> file 2", "contents 2"])

      it "parses two entries" $ oT testParse `parseSatisfies` ((== 2) . length . archiveEntries)
      it "parses the first filename" $ oT testParse `parseSatisfies` ((== "file 1") . entryFile . entryData . head . archiveEntries)
      it "parses the first contents" $ oT testParse `parseSatisfies` ((== Just "contents 1\n\n") . entryContent . entryData . head . archiveEntries)
      it "parses the second filename" $ oT testParse `parseSatisfies` ((== "file 2") . entryFile . entryData . last . archiveEntries)
      it "parses the second contents" $ oT testParse `parseSatisfies` ((== Just "contents 2\n") . entryContent . entryData . last . archiveEntries)

      it "allows an explicit parent directory" $ do
        let t p = p pArchive (T.unlines ["<===> dir/", "<===> dir/file", "contents"])
        t testParse `parseSatisfies` ((== Just "contents\n") . entryContent . entryData . last . archiveEntries)

      it "parses contents without a newline" $ do
        -- TODO: does not match referende implementation
        let t p = p pArchive "<===> file 1\ncontents 1\n<===> file 2\ncontents 2"
        t testParse `parseSatisfies` ((== Just "contents 1\n") . entryContent . entryData . head . archiveEntries)

      it "parses content with boundary like sequences" $ do
        let t p = p pArchive (T.unlines ["<===> file 1", "<==>", "inline <===>", "<====>", "<===> file 2", "contents"])
        t testParse `parseSatisfies` ((== Just (T.unlines ["<==>", "inline <===>", "<====>"])) . entryContent . entryData . head . archiveEntries)

      describe "with a comment" $ do
        let t p = p pArchive (T.unlines ["<===> file 1", "contents 1", "<===>", "comment", "<===> file 2", "contents 2"])

        it "parses two entries" $ t testParse `parseSatisfies` ((== 2) . length . archiveEntries)
        it "parses the first filename" $ t testParse `parseSatisfies` ((== "file 1") . entryFile . entryData . head . archiveEntries)
        it "parses the first contents" $ t testParse `parseSatisfies` ((== Just "contents 1\n") . entryContent . entryData . head . archiveEntries)
        it "parses the second filename" $ t testParse `parseSatisfies` ((== "file 2") . entryFile . entryData . last . archiveEntries)
        it "parses the second contents" $ t testParse `parseSatisfies` ((== Just "contents 2\n") . entryContent . entryData . last . archiveEntries)
        it "parses the comment" $ t testParse `parseSatisfies` ((== Just "comment\n") . entryComment . last . archiveEntries)

  it "parses a file that only contains a comment" $
    testParse pArchive (T.unlines ["<===>", "contents"]) `parseSatisfies` ((== Just "contents\n") . archiveComment)

  it "parses a file that only contains a comment with boundary-like sequences" $
    testParse pArchive (T.unlines ["<===>", "<==>", "inline <===>", "<====>"]) `parseSatisfies` ((== Just (T.unlines ["<==>", "inline <===>", "<====>"])) . archiveComment)

  describe "with a file and a trailing comment" $ do
    let t p = p pArchive (T.unlines ["<===> file", "contents", "", "<===>", "comment"])

    it "parses one entry" $ t testParse `parseSatisfies` ((== 1) . length . archiveEntries)
    it "parses the filename" $ t testParse `parseSatisfies` ((== "file") . entryFile . entryData . head . archiveEntries)
    it "parses the contents" $ t testParse `parseSatisfies` ((== Just "contents\n\n") . entryContent . entryData . head . archiveEntries)
    it "parses the comment" $ t testParse `parseSatisfies` ((== Just "comment\n") . archiveComment)

  describe "with a single directory" $ do
    let t p = p pArchive "<===> dir/\n"

    it "parses one entry" $ t testParse `parseSatisfies` ((== 1) . length . archiveEntries)
    it "parses a diretory and the filename" $ t testParse `parseSatisfies` ((== EntryDirectory "dir/") . entryData . head . archiveEntries)

  describe "forbids an HRX file that" $ do
    it "doesn't start with a boundary" $ testParse pArchive `shouldFailOn` "file\n"
    it "starts with an unclosed boundary" $ testParse pArchive `shouldFailOn` "<== file\n"
    it "starts with an unopened boundary" $ testParse pArchive `shouldFailOn` "==> file\n"
    it "starts with a malformed boundary" $ testParse pArchive `shouldFailOn` "<> file\n"
    it "has a directory with contents" $ testParse pArchive `shouldFailOn` "<===> dir/\ncontents"
