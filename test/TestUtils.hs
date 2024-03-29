module TestUtils where

import Codec.Archive.HRX.Internal (Archive, pArchive)
import Data.Text (Text)
import Text.Megaparsec (ParseErrorBundle, Parsec, errorBundlePretty, parse)

-- | Test utility to run a parser on some input
testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse p = parse p ""

parseFile :: (Applicative f) => Text -> f Archive
parseFile t = case parse pArchive "" t of
  Right out -> pure out
  Left err -> error $ errorBundlePretty err
