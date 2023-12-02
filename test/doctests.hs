module Main where

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-XNamedFieldPuns",
      "-XOverloadedStrings",
      "-isrc",
      "lib/Codec/Archive/HRX.hs",
      "lib/Codec/Archive/HRX/Internal.hs"
    ]
