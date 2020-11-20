{-# LANGUAGE OverloadedStrings #-}

module HRX.Parser (parse, ParserError, Archive (..)) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

newtype ParserError = ParserError String deriving (Show, Eq)

data Archive = Archive
  { archiveComment :: Maybe Comment,
    archiveEntries :: [Text]
  }
  deriving (Show, Eq)

data Comment = Comment {commentWidth :: Int, commentText :: Text} deriving (Show, Eq)

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline = not . isNewline

pBoundary :: Parser Int
pBoundary = do
  void (char '<')
  bounds <- T.length <$> takeWhile1P (Just "=") (== '=')
  void (char '>')
  void eol
  return $ bounds + 2

pComment :: Parser Comment
pComment = do
  commentWidth <- pBoundary
  commentText <- takeWhile1P (Just "Comment") notNewline
  return Comment {commentWidth, commentText}

pArchive :: Parser Archive
pArchive = do
  archiveComment <- optional pComment
  let archiveEntries = [] :: [Text]
  return Archive {archiveComment, archiveEntries}

parse :: String -> Text -> Either ParserError Archive
parse file input = case runParser pArchive file input of
  Right archive -> Right archive
  Left err -> Left $ ParserError $ errorBundlePretty err
