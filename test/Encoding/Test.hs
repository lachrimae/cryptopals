{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Encoding.Test
    ( testHex
    , testBase64
    ) where

import "hspec" Test.Hspec
import "QuickCheck" Test.QuickCheck
import qualified "bytestring" Data.ByteString as B
import qualified "text" Data.Text as T
import "cryptopals" Encoding
import "base" Data.Word (Word8)

instance Arbitrary B.ByteString where
    arbitrary = B.pack `fmap` arbitrary

instance Arbitrary Hex where
    arbitrary = fmap (Hex . T.replicate 2 . T.pack)
              . liftArbitrary
              $ frequency [(10, choose ('0', '9')), (6, choose ('A', 'F'))]

instance Arbitrary Base64 where
    arbitrary = fmap (Base64 . T.pack . pad)
              . liftArbitrary
              $ frequency
              [ (26, choose ('a', 'z'))
              , (26, choose ('A', 'Z'))
              , (10, choose ('0', '0'))
              , (1,  choose ('+', '+'))
              , (1,  choose ('/', '/')) ]
                  where
                      pad xs
                        | length xs `mod` 4 == 3 = xs ++ "A=="
                        | length xs `mod` 4 == 2 = xs ++ "=="
                        | length xs `mod` 4 == 1 = xs ++ "="
                        | length xs `mod` 4 == 0 = xs

testHex :: Spec
testHex = do
    describe "fromHex and toHex" $ do
        it "work on single cells" $ match
            fromHex
            toHex
            [ ("00", 0)
            , ("09", 9)
            , ("0B", 11)
            , ("2C", 44)
            , ("FF", 255) ]
        it "satisfy fromHex . toHex == id" $
            property (\b -> b == (fromHex (toHex   b)))
        it "satisfy toHex . fromHex == id" $
            property (\h -> h == (toHex   (fromHex h)))

testBase64 :: Spec
testBase64 = do
    describe "fromBase64 and toBase64" $ do
        it "satisfy fromBase64 . toBase64 == id" $
            property (\b   -> b   == (fromBase64 (toBase64 b  )))
        it "satisfy toBase64 . fromBase64 == id" $
            property (\b64 -> b64 == (fromBase64 (toBase64 b64)))

match :: (Show a, Eq a)
      => (a -> B.ByteString)
      -> (B.ByteString -> a)
      -> [(a, Word8)]
      -> Expectation
match f g = foldl (\b a -> b >> f' a >> g' a) (return ())
    where
        f' (h, w) = f h            `shouldBe` B.pack [w]
        g' (h, w) = g (B.pack [w]) `shouldBe` h
