{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HRX.Parser where

import Control.Monad (void)
import Data.Char (ord)
import Data.Functor (($>))
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char (eol, hspace1, string)

type Parser = Parsec ParserError Text

data ParserError
  = ParserError Text
  | PathError Text
  | BoundaryWidthError
  | DuplicateFileError Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent (ParserError e) = T.unpack e
  showErrorComponent (PathError e) = T.unpack e ++ " is not a valid path"
  showErrorComponent BoundaryWidthError = "Boundary did not match first boundary in archive"
  showErrorComponent (DuplicateFileError e) = T.unpack e ++ " defined twice"

data Archive = Archive
  { -- | Width of boundary in `=`
    archiveBoundary :: Int,
    -- | Entries in the file
    archiveEntries :: [(Path, Entry)],
    -- | Optional final comment
    archiveComment :: Maybe Text
  }
  deriving (Show, Eq)

newtype Path = Path Text deriving (Show, Eq, Ord)

isDir :: Path -> Bool
isDir (Path path) = T.last path == '/'

data Entry = Entry
  { entryData :: EntryType,
    entryComment :: Maybe Text
  }
  deriving (Show, Eq)

data EntryType
  = EntryFile {entryContent :: Maybe Text}
  | EntryDirectory
  deriving (Show, Eq)

getEntries :: [EntryType] -> ([EntryType], [EntryType])
getEntries ents = entries' ents ([], [])
  where
    entries' (e@EntryDirectory {} : es) (dirs, files) = entries' es (e : dirs, files)
    entries' (e@EntryFile {} : es) (dirs, files) = entries' es (dirs, e : files)
    entries' [] (d, f) = (d, f)

isNewline :: Char -> Bool
isNewline x = x == '\n'

notNewline :: Char -> Bool
notNewline = not . isNewline

isPathChar :: Char -> Bool
isPathChar p =
  not $
    chr <= 31 -- Control code between U+0000 through U+001F
      || chr == 127 -- U+007F DELETE
      || chr == 47 -- U+002F SOLIDUS
      || chr == 58 -- U+003A COLON
      || chr == 92 -- U+005C REVERSE SOLIDUS
  where
    chr = ord p

pText :: Text -> Parser Text
pText b = do
  notFollowedBy (chunk b <|> eof $> "")
  takeWhileP Nothing notNewline <> (eol <|> eof $> "") <?> "File body"

pBody :: Text -> Parser Text
pBody b = T.concat <$> some (pText b)

pPathComponent :: Parser Text
pPathComponent = do
  comp <- takeWhile1P Nothing isPathChar
  if valid comp then return comp else customFailure $ PathError comp
  where
    valid x = x /= "." && x /= ".."

pSlash :: Parser Text
pSlash = string "/" <* notFollowedBy (string "/") <?> "Slash"

pPath :: Parser Path
pPath = do
  root <- pPathComponent <?> "Path root"
  rest <- many (pPathComponent <|> pSlash) <?> "Path rest"
  return $ Path (root <> T.concat rest)

pBoundary :: Parser Text
pBoundary = string "<" <> takeWhile1P Nothing (== '=') <> string ">" <?> "Boundary"

pComment :: Text -> Parser Text
pComment b = do
  bC <- pBoundary
  if bC /= b
    then customFailure BoundaryWidthError
    else do
      void eol
      pBody bC

pEntry :: Text -> Parser (Path, Entry)
pEntry b = do
  entryComment <- (optional . try $ pComment b) <?> "Entry comment"
  entryBoundary <- pBoundary <?> "Entry boundary"
  if entryBoundary /= b
    then customFailure BoundaryWidthError
    else do
      void hspace1
      path <- pPath <?> "Directory path"
      if isDir path
        then do
          void (some eol <?> "Directory")
          return (path, Entry {entryComment, entryData = EntryDirectory})
        else do
          entryData <- pFile entryBoundary <?> "File"
          return (path, Entry {entryComment, entryData})

pFile :: Text -> Parser EntryType
pFile b = do
  void eol
  entryContent <- optional (pBody b <?> "File content")
  return EntryFile {entryContent}

pArchive :: Parser Archive
pArchive = do
  b <- lookAhead pBoundary <|> eof $> ""
  archiveEntries <- many (try $ pEntry b) <?> "Entries"
  archiveComment <- (optional . try $ pComment b) <?> "Archive comment"
  void eof <?> "End of archive"
  let archive = Archive {archiveBoundary = T.length b - 2, archiveComment, archiveEntries}
  if validArchive archive
    then return archive
    else customFailure $ ParserError "Duplicate file/directory"

validArchive :: Archive -> Bool
validArchive archive = length (nub paths) == length paths
  where
    paths = map fst (archiveEntries archive)
