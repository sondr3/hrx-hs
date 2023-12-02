module Codec.Archive.HRX.Internal
  ( readArchive,
    writeArchive,
    toHRX,
    toHRX',
    fromHRX,
    fromHRX',
    findEntry,
    findEntriesGlob,
    readEntry,
    liftEither,
    lastComment,
    module Codec.Archive.HRX.Parser,
  )
where

import Codec.Archive.HRX.Parser
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.FilePattern (FilePattern, (?==))
import Text.Megaparsec (ParseErrorBundle)
import Text.Megaparsec qualified as M

-- $setup
-- >>> archive = liftEither $ fromHRX "<===> file\nfile contents\n<===> dir/\n<===>\ncomment contents\n<===> super/sub\nsub contents\n<===> very/deeply/\n<===> very/deeply/nested/file\nnested contents\n<===> last\nthe last file"

-- | Read an archive from the file system and parse it.
--
-- @since 0.1.0
readArchive :: FilePath -> IO (Either ParserError Archive)
readArchive path = do
  file <- TIO.readFile path
  case parse path file of
    Right a -> return $ Right a
    Left err -> return $ Left $ ParserError $ T.pack $ M.errorBundlePretty err

-- | Write an archive to a given file. Will create any intermediate folders
-- if required.
--
-- @since 0.1.0
writeArchive :: Archive -> FilePath -> IO ()
writeArchive archive path = createAndWriteFile path (toHRX archive)

-- | Get the last comment of the archive, if any.
--
-- @since 0.1.0
lastComment :: Archive -> Maybe Text
lastComment = archiveComment

-- | Attempt to parse some 'Text' to an 'Archive'.
--
-- @since 0.1.0
--
-- ==== __Examples__
--
-- >>> fromHRX ""
-- Right (Archive {archiveBoundary = 0, archiveEntries = [], archiveComment = Nothing})
--
-- >>> fromHRX "<===>\ncomment\n<===> file\ncontents\n"
-- Right (Archive {archiveBoundary = 3, archiveEntries = [Entry {entryData = EntryFile {entryContent = Just "contents\n"}, entryPath = "file", entryComment = Just "comment\n"}], archiveComment = Nothing})
fromHRX :: Text -> Either ParserError Archive
fromHRX content = do
  case parse "" content of
    Right a -> Right a
    Left err -> Left $ ParserError $ T.pack $ M.errorBundlePretty err

-- | 'fromHRX', but takes a 'String' instead of 'Text'.
--
-- @since 0.1.0
fromHRX' :: String -> Either ParserError Archive
fromHRX' content = fromHRX $ T.pack content

-- | Serialize an 'Archive' to a plain-text HRX file.
--
-- @since 0.1.0
--
-- ==== __Examples__
--
-- >>> toHRX (Archive {archiveBoundary = 3, archiveEntries = [], archiveComment = Just "A HRX file may consist of only a comment and nothing else.\n"})
-- "<===>\nA HRX file may consist of only a comment and nothing else.\n"
--
-- >>> toHRX (Archive {archiveBoundary = 0, archiveEntries = [], archiveComment = Nothing})
-- ""
toHRX :: Archive -> Text
toHRX archive =
  T.concat $
    map (`printEntry` boundary) (archiveEntries archive)
      <> [printComment (archiveComment archive) boundary]
  where
    boundary = "<" <> T.replicate (archiveBoundary archive) "=" <> ">"

-- | 'toHRX', but takes a 'String' instead of 'Text'.
--
-- @since 0.1.0
toHRX' :: Archive -> String
toHRX' archive = T.unpack $ toHRX archive

-- | Look for a single entry by looking up a path in an archive. This function
-- does not validate the path, it simply returns 'Nothing' is there is no entry.
--
-- __NOTE:__ If no file is found at a path, it will try to look up a directory
-- instead by appending \'@/@\' to the path. This behaviour matches the
-- reference implementation.
--
-- @since 0.1.0
--
-- ==== __Examples__
--
-- >>> findEntry "file" archive
-- Just (Entry {entryData = EntryFile {entryContent = Just "file contents\n"}, entryPath = "file", entryComment = Nothing})
--
-- >>> findEntry "dir/" archive
-- Just (Entry {entryData = EntryDirectory, entryPath = "dir/", entryComment = Nothing})
--
-- >>> findEntry "dir" archive
-- Just (Entry {entryData = EntryDirectory, entryPath = "dir/", entryComment = Nothing})
findEntry :: Text -> Archive -> Maybe Entry
findEntry path archive
  | isNothing findEntry' = findDir
  | otherwise = findEntry'
  where
    findDir = find (\x -> entryPath x == (path <> "/")) (archiveEntries archive)
    findEntry' = find (\x -> entryPath x == path) (archiveEntries archive)

-- | Finds all matching entries in an archive that matches the given pattern.
--
-- __NOTE:__ This only returns directories if the pattern ends with \'@/@\' or
-- includes \'@**@\'.
--
-- @since 0.1.0
--
-- ==== __Examples__
--
-- >>> findEntriesGlob "" archive
-- []
--
-- >>> findEntriesGlob "*" archive
-- [Entry {entryData = EntryFile {entryContent = Just "file contents\n"}, entryPath = "file", entryComment = Nothing},Entry {entryData = EntryFile {entryContent = Just "the last file"}, entryPath = "last", entryComment = Nothing}]
--
-- >>> findEntriesGlob "very/**/*" archive
-- [Entry {entryData = EntryDirectory, entryPath = "very/deeply/", entryComment = Nothing},Entry {entryData = EntryFile {entryContent = Just "nested contents\n"}, entryPath = "very/deeply/nested/file", entryComment = Nothing}]
findEntriesGlob :: FilePattern -> Archive -> [Entry]
findEntriesGlob glob archive = mapMaybe (entryGlob glob) (archiveEntries archive)

-- | Read the contents of a file at the given path in an archive.
--
-- __NOTE:__ This will only return content for a file, not a directory.
--
-- @since 0.1.0
--
-- ==== __Examples__
--
-- >>> readEntry "file" archive
-- Just "file contents\n"
--
-- >>> readEntry "dir/" archive
-- Nothing
--
-- >>> readEntry "super/sub" archive
-- Just "sub contents\n"
readEntry :: Text -> Archive -> Maybe Text
readEntry path archive = case find (\x -> entryPath x == path) (archiveEntries archive) of
  Just (Entry (EntryFile content) _ _) -> content
  _ -> Nothing

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

-- | Only ever used in tests.
liftEither :: Either a p -> p
liftEither (Right r) = r
liftEither (Left _) = error "liftEither called with Left"
