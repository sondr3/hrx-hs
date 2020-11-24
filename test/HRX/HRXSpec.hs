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
      testParse pArchive "<===> file\ncontents" `parseSatisfies` ((== "contents") . entryContent . entryData . head . archiveEntries)

    describe "with a single file" $ do
      let t p = p pArchive (T.unlines ["<===> file", "contents"])

      it "parses one entry" $ t testParse `parseSatisfies` ((== 1) . length . archiveEntries)
      it "parses the filename" $ t testParse `parseSatisfies` ((== "file") . entryFile . entryData . head . archiveEntries)
      it "parses the contents" $ t testParse `parseSatisfies` ((== "contents\n") . entryContent . entryData . head . archiveEntries)

    describe "parses contents with boundary-like sequences" $ do
      let t p = p pArchive (T.unlines ["<===> file", "<==>", "inline <===>", "<====>"])

      it "parses the contents" $
        t testParse `parseSatisfies` ((== T.unlines ["<==>", "inline <===>", "<====>"]) . entryContent . entryData . head . archiveEntries)

    describe "with a comment" $ do
      let t p = p pArchive (T.unlines ["<===>", "comment", "<===> file", "contents"])

      it "parses one entry" $ t testParse `parseSatisfies` ((== 1) . length . archiveEntries)
      it "parses the filename" $ t testParse `parseSatisfies` ((== "file") . entryFile . entryData . head . archiveEntries)
      it "parses the contents" $ t testParse `parseSatisfies` ((== "contents\n") . entryContent . entryData . head . archiveEntries)
      it "parses the comment" $ t testParse `parseSatisfies` ((== Just "comment") . entryComment . head . archiveEntries)

    describe "with multiple files" $ do
      let t p = p pArchive (T.unlines ["<===> file 1", "contents 1", "<===> file 2", "contents 2"])

      it "parses two entries" $ t testParse `parseSatisfies` ((== 2) . length . archiveEntries)
      it "parses the first filename" $ t testParse `parseSatisfies` ((== "file 1") . entryFile . entryData . head . archiveEntries)
      it "parses the first contents" $ t testParse `parseSatisfies` ((== "contents 1\n") . entryContent . entryData . head . archiveEntries)
      it "parses the second filename" $ t testParse `parseSatisfies` ((== "file 2") . entryFile . entryData . head . tail . archiveEntries)
      it "parses the second contents" $ t testParse `parseSatisfies` ((== "contents 2\n") . entryContent . entryData . head . tail . archiveEntries)
