module HRX.HRXSpec (spec) where

import qualified Data.Text as T
import HRX.Internal
import HRX.TestUtils (liftEither, testParse)
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "convert from and back again" $ do
    let content = T.unlines ["<===> file 1", "contents 1", "<===>", "comment", "<===> file 2", "contents 2"]
        parsed = liftEither $ testParse pArchive content

    it "serializes" $ liftEither (fromHRX content) `shouldBe` parsed
    it "losslessly converts" $ toHRX (liftEither $ fromHRX content) `shouldBe` content
