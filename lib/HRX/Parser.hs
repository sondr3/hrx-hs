{-# LANGUAGE OverloadedStrings #-}

module HRX.Parser (parse, ParserError, Archive (..), Entry (..)) where

import Control.Monad (void)
import Control.Monad.State (State, evalState, get, put)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char (alphaNumChar, char, eol, string)

type Parser = ParsecT Void Text (State (Maybe Int))

newtype ParserError = ParserError String deriving (Show, Eq)

newtype Archive = Archive
  { archiveEntries :: [Entry]
  }
  deriving (Show, Eq)

data Entry = Entry
  { entryFile :: Text,
    entryContent :: Text
  }
  deriving (Show, Eq)

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline = not . isNewline

pBoundary :: Parser ()
pBoundary = do
  width <- T.length <$> (char '<' *> takeWhile1P Nothing (== '=') <* char '>')
  reqWidth <- get
  case reqWidth of
    Nothing -> put (Just $ width + 2)
    Just w -> if width + 2 /= w then failure Nothing Set.empty else pure ()

pComment :: Parser ()
pComment = do
  void pBoundary
  void $ char '\n'
  void $ takeWhile1P (Just "Comment") notNewline

pEntry :: Parser Entry
pEntry = do
  void (optional . try $ do pComment)
  void pBoundary
  void $ char ' '
  entryFile <- takeWhile1P Nothing notNewline
  void eol
  entryContent <- T.pack <$> many (alphaNumChar <|> char ' ' <|> char '\n') <* notFollowedBy (string "\n<=")

  return Entry {entryFile, entryContent}

pArchive :: Parser Archive
pArchive = do
  void (optional . try $ do pComment)
  archiveEntries <- many pEntry
  return Archive {archiveEntries}

parse :: String -> Text -> Either ParserError Archive
parse file input = case evalState (runParserT pArchive file input) Nothing of
  Right archive -> Right archive
  Left err -> do
    trace (errorBundlePretty err) (pure ())
    Left $ ParserError "Could not parse input"
