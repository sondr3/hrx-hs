{-# LANGUAGE OverloadedStrings #-}

module HRX
  ( readArchive,
    toHRX,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HRX.Internal (Archive, Entry (..), EntryType (..), ParserError, Path (..), archiveBoundary, archiveEntries, pArchive)
import Text.Megaparsec (ParseErrorBundle)
import qualified Text.Megaparsec as M

readArchive :: FilePath -> IO Archive
readArchive path = do
  file <- TIO.readFile path
  case parse path file of
    Right a -> return a
    Left err -> error $ M.errorBundlePretty err

toHRX :: Archive -> Text
toHRX archive = T.concat $ map (\x -> uncurry printEntry x boundary) (archiveEntries archive)
  where
    boundary = "<" <> T.replicate (archiveBoundary archive) "=" <> ">"

printEntry :: Path -> Entry -> Text -> Text
printEntry p (Entry (EntryFile content) comment) b = printComment comment b <> printFile p content b
printEntry p (Entry EntryDirectory comment) b = printComment comment b <> printDirectory p b

printDirectory :: Path -> Text -> Text
printDirectory p b = b <> " " <> printPath p <> "\n"

printFile :: Path -> Maybe Text -> Text -> Text
printFile _ Nothing _ = ""
printFile p (Just content) b = b <> " " <> printPath p <> "\n" <> content

printComment :: Maybe Text -> Text -> Text
printComment Nothing _ = ""
printComment (Just comment) b = b <> "\n" <> comment

printPath :: Path -> Text
printPath (Path p) = p

parse :: FilePath -> Text -> Either (ParseErrorBundle Text ParserError) Archive
parse path input = case M.parse pArchive path input of
  Right archive -> Right archive
  Left err -> Left err
