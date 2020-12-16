module Codec.Archive.HRX.HRXSpec (spec) where

import Codec.Archive.HRX.Internal
import qualified Data.Text as T
import Test.Hspec
import TestUtils (testParse)

spec :: Spec
spec = parallel $ do
  describe "convert from and back again" $ do
    let content = T.unlines ["<===> file 1", "contents 1", "<===>", "comment", "<===> file 2", "contents 2"]
        parsed = liftEither $ testParse pArchive content

    it "serializes" $ liftEither (fromHRX content) `shouldBe` parsed
    it "losslessly converts" $ toHRX (liftEither $ fromHRX content) `shouldBe` content
