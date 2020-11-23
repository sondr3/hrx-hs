{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HRX.Parser where

import Control.Monad (void)
import Control.Monad.Identity (Identity)
import Control.Monad.State (State, StateT, evalState, get, put)
import Data.Char (ord)
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Debug (dbg)

type Parser' = Parsec ParserError Text

type Parser = ParsecT Void Text (State (Maybe Text))

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
  { entryFile :: Text,
    entryContent :: Text,
    entryComment :: Maybe Text
  }
  deriving (Show, Eq)

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline = not . isNewline

notBoundary :: Text -> Text -> Bool
notBoundary b t = b /= t || ("\n" <> b) /= t

isPath :: Char -> Bool
isPath c = isPathChar c || c == '/'

isPathComponent :: Char -> Bool
isPathComponent = isPathChar

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

pPathComponent :: Parser' Text
pPathComponent = do
  comp <- takeWhile1P Nothing isPathChar
  if valid comp then return comp else customFailure $ PathError comp
  where
    valid x = x /= "." && x /= ".."

pSlash :: Parser' Text
pSlash = string "/" <* notFollowedBy (string "/")

pPath :: Parser' Text
pPath = do
  -- path <- takeWhile1P (Just "path") isPathComponent <> takeWhile1P Nothing isPath
  root <- pPathComponent
  rest <- some (pSlash <|> pPathComponent) <* notFollowedBy (string "/")
  return $ root <> T.concat rest

pBoundary :: Parser Text
pBoundary = do
  eqs <- char '<' *> takeWhile1P Nothing (== '=') <* char '>'
  let boundary = "<" <> eqs <> ">"
  archiveBoundary <- get
  case archiveBoundary of
    Nothing -> do
      put (Just boundary)
      void (trace (T.unpack boundary) (pure ()))
      return boundary
    Just w -> if boundary /= w then failure Nothing Set.empty else return w

pComment :: Parser Text
pComment = do
  void pBoundary
  void $ char '\n'
  takeWhile1P (Just "Comment") notNewline <* char '\n'

pEntryContent :: Parser Text
pEntryContent = do
  archiveBoundary <- get
  case archiveBoundary of
    Nothing -> T.pack <$> many anySingle
    -- Just b -> T.pack <$> manyTill anySingle (lookAhead (try (string b)))
    -- Just b -> T.pack <$> manyTill anySingle (lookAhead (string b)) <?> "Entry content"
    Just b -> T.pack <$> manyTill anySingle (lookAhead (string b <|> string ("\n" <> b)))

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
  archiveEntries <- many pEntry
  archiveComment <- optional . try $ do pComment
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
