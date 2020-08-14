{-# LANGUAGE PackageImports #-}

module Spec where

import "hspec" Test.Hspec
import "QuickCheck" Test.QuickCheck
import Control.Exception (evaluate)
import Encoding.Test
import Scoring.Test
import Bitwise.Test

main :: IO ()
main = hspec $ do
    testXor
    testHex
    testBase64
--    scoringTests
