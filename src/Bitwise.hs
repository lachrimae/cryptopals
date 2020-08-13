{-# LANGUAGE PackageImports #-}

module Bitwise
    ( xor
    ) where

import qualified "bytestring" Data.ByteString as B (ByteString, pack, length, zipWith)
import "base" Data.Word (Word8)

xor' :: Word8 -> Word8 -> Word8
xor' w1 w2 = sum $ map addDigit $ [0..7]
    where addDigit n = 2^n * ((w1 `div` 2^n + w2 `div` 2^n) `mod` 2)

xor :: B.ByteString -> B.ByteString -> Maybe B.ByteString
xor w1 w2
  | B.length w1 /= B.length w2 = Nothing
  | otherwise                  = Just . B.pack $ B.zipWith xor' w1 w2
