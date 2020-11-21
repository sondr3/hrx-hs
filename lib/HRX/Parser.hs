{-# LANGUAGE OverloadedStrings #-}

module HRX.Parser (parse, ParserError, Archive (..)) where

import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Debug.Trace (trace)
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

newtype ParserError = ParserError String deriving (Show, Eq)

newtype Archive = Archive
  { archiveEntries :: [Entry]
  }
  deriving (Show, Eq)

data Comment = Comment {commentWidth :: Int, commentText :: Text} deriving (Show, Eq)

data Entry = Entry
  { entryFile :: Text,
    entryContent :: Text
  }
  deriving (Show, Eq)

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline = not . isNewline

pBoundary :: Parser Int
pBoundary = do
  width <- T.length <$> (char '<' *> takeWhile1P Nothing (== '=') <* char '>')
  return $ width + 2

pComment :: Parser Comment
pComment = do
  commentWidth <- pBoundary
  void $ char '\n'
  commentText <- takeWhile1P (Just "Comment") notNewline
  return Comment {commentWidth, commentText}

pEntry :: Parser Entry
pEntry = do
  void (try $ do pComment)
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
parse file input = case runParser pArchive file input of
  Right archive -> Right archive
  Left err -> do
    trace (errorBundlePretty err) (pure ())
    Left $ ParserError "Could not parse input"
