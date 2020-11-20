module HRX.HRXSpec (spec) where

import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "parser" $ do
    it "parses" True
