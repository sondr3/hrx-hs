module HRX.TestUtils where

import Data.Text (Text)
import HRX.Internal (Archive, parse)

-- | Parse a test case and return the parsed archive or throw a generic error.
testParser :: Text -> Archive
testParser input = case parse "" input of
  Right archive -> archive
  Left _ -> error "Could not parse input"

-- | Parse a test case and return either the archive or () if it failed.
testParser' :: Text -> Either () Archive
testParser' input = case parse "" input of
  Right archive -> Right archive
  Left _ -> Left ()
