{-# LANGUAGE PackageImports #-}

module Bitwise.Test
    ( testXor
    ) where

import "hspec" Test.Hspec
import "QuickCheck" Test.QuickCheck
import "bytestring" Data.ByteString
import qualified "bytestring" Data.ByteString as B (reverse)
import "cryptopals" Bitwise

testXor :: Spec
testXor = describe "xor" $ do
    it "is its own inverse" $ do
        (xor x =<< xor x y) `shouldBe` Just y
    it "has expected behaviour on single bits" $ do
        zero `xor` zero `shouldBe` Just zero
        zero `xor` one  `shouldBe` Just one
        one  `xor` zero `shouldBe` Just one
        one  `xor` one  `shouldBe` Just zero
    where
        x = pack [0..255]
        y = B.reverse x
        one = pack [1]
        zero = pack [0]
