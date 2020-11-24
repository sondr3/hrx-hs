{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HRX.Parser where

import Control.Monad (void)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Internal.Search (indices)
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char (eol, hspace1, string)
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec ParserError Text

showPosStack :: [String] -> String
showPosStack = intercalate ", " . fmap ("in " ++)

data ParserError
  = ParserError Text
  | PathError Text
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ParserError where
  showErrorComponent (ParserError e) = T.unpack e ++ "generic error happened"
  showErrorComponent (PathError e) = T.unpack e ++ " is not a valid path"

data Archive = Archive
  { archiveComment :: Maybe Text,
    archiveEntries :: [Entry]
  }
  deriving (Show, Eq)

data Entry = Entry
  { entryBoundary :: Text,
    entryData :: EntryType,
    entryComment :: Maybe Text
  }
  deriving (Show, Eq)

data EntryType
  = EntryFile {entryFile :: Text, entryContent :: Text}
  | EntryDirectory Text
  deriving (Show, Eq)

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline = not . isNewline

notBoundary :: Text -> Text -> Bool
notBoundary b t = b /= t || ("\n" <> b) /= t

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

pPathComponent :: Parser Text
pPathComponent = do
  comp <- takeWhile1P Nothing isPathChar
  if valid comp then return comp else customFailure $ PathError comp
  where
    valid x = x /= "." && x /= ".."

pSlash :: Parser Text
pSlash = string "/" <* notFollowedBy (string "/")

pPath :: Parser Text
pPath = do
  root <- dbg "root" pPathComponent <?> "Path root"
  rest <- dbg "rest" (many (pPathComponent <|> pSlash) <?> "Path rest")
  return $ root <> T.concat rest

pBoundary :: Parser Text
pBoundary = string "<" <> takeWhile1P Nothing (== '=') <> string ">"

pComment :: Parser Text
pComment = do
  void pBoundary
  void eol
  takeWhile1P (Just "Comment") notNewline <* eol

pEntry :: Parser Entry
pEntry = do
  entryComment <- dbg "comment" $ optional . try $ pComment
  entryBoundary <- dbg "boundary" pBoundary
  void hspace1
  path <- dbg "entry path" pPath <?> "Directory path"
  if T.last path == '/'
    then do
      entryData <- pDirectory path
      return Entry {entryComment, entryBoundary, entryData}
    else do
      entryData <- pFile entryBoundary path
      return Entry {entryComment, entryBoundary, entryData}

pDirectory :: Text -> Parser EntryType
pDirectory p = do
  void $ some eol
  return $ EntryDirectory p

pFile :: Text -> Text -> Parser EntryType
pFile b p = do
  void eol
  input <- getInput
  let (entryContent, rest) = getContent input
  setInput rest
  return EntryFile {entryFile = p, entryContent}
  where
    getContent txt = case indices (b) txt of
      [] -> (txt, "")
      (x : _) -> T.splitAt x txt

pArchive :: Parser Archive
pArchive = do
  archiveEntries <- dbg "entries" $ many pEntry
  archiveComment <- dbg "archive comment" $ optional . try $ do pComment
  return Archive {archiveComment, archiveEntries}
