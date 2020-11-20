module HRX.TestUtils (testParser, testParser') where

import Data.Text (Text)
import HRX (Archive, parse)

testParser :: Text -> Archive
testParser input = case parse "" input of
  Right archive -> archive
  Left _ -> error "Could not parse input"

testParser' :: Text -> Either String Archive
testParser' input = case parse "" input of
  Right archive -> Right archive
  Left _ -> Left "Could not parse input"
