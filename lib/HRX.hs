module HRX (parse) where

import Data.Text (Text)
import HRX.Internal (Archive, ParserError, pArchive)
import Text.Megaparsec (ParseErrorBundle)
import qualified Text.Megaparsec as M

parse :: FilePath -> Text -> Either (ParseErrorBundle Text ParserError) Archive
parse path input = case M.parse pArchive path input of
  Right archive -> Right archive
  Left err -> Left err
