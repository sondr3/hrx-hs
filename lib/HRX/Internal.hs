module HRX.Internal
  ( readArchive,
    writeArchive,
    toHRX,
    fromHRX,
    entriesGlob,
    module HRX.Parser,
  )
where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HRX.Parser
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.FilePattern (FilePattern, (?==))
import Text.Megaparsec (ParseErrorBundle)
import qualified Text.Megaparsec as M

readArchive :: FilePath -> IO (Either ParserError Archive)
readArchive path = do
  file <- TIO.readFile path
  case parse path file of
    Right a -> return $ Right a
    Left err -> return $ Left $ ParserError $ T.pack $ M.errorBundlePretty err

writeArchive :: Archive -> FilePath -> IO ()
writeArchive archive path = createAndWriteFile path (toHRX archive)

fromHRX :: Text -> Either ParserError Archive
fromHRX content = do
  case parse "" content of
    Right a -> Right a
    Left err -> Left $ ParserError $ T.pack $ M.errorBundlePretty err

toHRX :: Archive -> Text
toHRX archive =
  T.concat $
    map (`printEntry` boundary) (archiveEntries archive)
      <> [printComment (archiveComment archive) boundary]
  where
    boundary = "<" <> T.replicate (archiveBoundary archive) "=" <> ">"

entriesGlob :: FilePattern -> Archive -> [Entry]
entriesGlob glob archive = mapMaybe (entryGlob glob) (archiveEntries archive)

printEntry :: Entry -> Text -> Text
printEntry (Entry (EntryFile content) p comment) b = printComment comment b <> printFile p content b
printEntry (Entry EntryDirectory p comment) b = printComment comment b <> printDirectory p b

printDirectory :: Text -> Text -> Text
printDirectory p b = b <> " " <> p <> "\n"

printFile :: Text -> Maybe Text -> Text -> Text
printFile p content b = b <> " " <> p <> "\n" <> fromMaybe "" content

printComment :: Maybe Text -> Text -> Text
printComment Nothing _ = ""
printComment (Just comment) b = b <> "\n" <> comment

entryGlob :: FilePattern -> Entry -> Maybe Entry
entryGlob glob e@(Entry _ path _) = if glob ?== T.unpack path then Just e else Nothing

parse :: FilePath -> Text -> Either (ParseErrorBundle Text ParserError) Archive
parse path input = case M.parse pArchive path input of
  Right archive -> Right archive
  Left err -> Left err

createAndWriteFile :: FilePath -> Text -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  TIO.writeFile path content
