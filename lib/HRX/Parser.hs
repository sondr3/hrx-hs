{-# LANGUAGE OverloadedStrings #-}

module HRX.Parser where

import Control.Monad (void)
import Control.Monad.Identity (Identity)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char (char, eol, string)

type Parser = ParsecT Void Text (State (Maybe Text))

newtype ParserError = ParserError String deriving (Show, Eq)

data Archive = Archive
  { archiveComment :: Maybe Text,
    archiveEntries :: [Entry]
  }
  deriving (Show, Eq)

data Entry = Entry
  { entryFile :: Text,
    entryContent :: Text,
    entryComment :: Maybe Text
  }
  deriving (Show, Eq)

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline = not . isNewline

pBoundary :: Parser Text
pBoundary = do
  eqs <- char '<' *> takeWhile1P Nothing (== '=') <* char '>'
  let boundary = "<" <> eqs <> ">"
  archiveBoundary <- get
  case archiveBoundary of
    Nothing -> do
      put (Just boundary)
      return boundary
    Just w -> if boundary /= w then failure Nothing Set.empty else return w

pComment :: Parser Text
pComment = do
  void pBoundary
  void $ char '\n'
  takeWhile1P (Just "Comment") notNewline

pEntryContent :: Parser Text
pEntryContent = do
  archiveBoundary <- get
  case archiveBoundary of
    Nothing -> T.pack <$> many anySingle
    Just b -> T.pack <$> many anySingle <* notFollowedBy (string b)

pEntry :: Parser Entry
pEntry = do
  entryComment <- optional . try $ do pComment <?> "Entry comment"
  void pBoundary <?> "Entry boundary"
  void (char ' ' <?> "Boundary space")
  entryFile <- takeWhile1P (Just "Entry filename") notNewline
  void eol
  entryContent <- pEntryContent

  return Entry {entryFile, entryContent, entryComment}

pArchive :: Parser Archive
pArchive = do
  archiveComment <- optional . try $ do pComment
  archiveEntries <- many pEntry
  return Archive {archiveComment, archiveEntries}

parse :: String -> Text -> Either ParserError Archive
parse file input = case evalState (runParserT pArchive file input) Nothing of
  Right archive -> Right archive
  Left err -> do
    trace (errorBundlePretty err) (pure ())
    Left $ ParserError "Could not parse input"

-- | Internal testing tool to use since 'parseTest' doesn't work with state.
parseTest' ::
  ( Show a,
    VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  ParsecT e s (StateT (Maybe m) Identity) a ->
  s ->
  IO ()
parseTest' parser input = case evalState (runParserT parser "" input) Nothing of
  Right x -> print x
  Left err -> putStrLn $ errorBundlePretty err
