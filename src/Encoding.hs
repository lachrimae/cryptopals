{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Encoding
    ( fromHex
    , toHex
    , fromBase64
    , toBase64
    , Hex(..)
    , Base64(..)
    ) where

import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Builder as BB
import qualified "text" Data.Text as T
import "containers" Data.Map (Map, fromList, keys, (!))
import "split" Data.List.Split (chunksOf)
import "base" Data.Char (toUpper)
import "base" Data.Word (Word8)
import "base" Data.String (IsString, fromString)
import "base" GHC.Exts (IsList)

newtype Hex = Hex T.Text
    deriving (Eq, Show, IsList)

newtype Base64 = Base64 T.Text
    deriving (Eq, Show, IsList)

instance IsString Hex where
    fromString s = if and [c `elem` (map fst hexDict) | c <- s']
                      then Hex . T.pack $ s'
                      else undefined
                     where s' = map toUpper s

instance IsString Base64 where
    fromString s = if and [c `elem` (map fst base64Dict) | c <- s]
                      then Base64 . T.pack $ s
                      else undefined

hexDict :: [(Char, Word8)]
hexDict = [('0', 0), ('1', 1),  ('2', 2),  ('3', 3),  ('4', 4),  ('5', 5),  ('6', 6),  ('7', 7),  ('8', 8),  ('9', 9),  ('A', 10),  ('B', 11),  ('C', 12),  ('D', 13),  ('E', 14),  ('F', 15)]

base64Dict :: [(Char, Word8)]
base64Dict
  = foldr func [] symbols
      where func :: Char -> [(Char, Word8)] -> [(Char, Word8)]
            func c cs = cs ++ [(c, fromIntegral $ length cs)]
            symbols = reverse ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/']

fromHexMap :: Map Char Word8
fromHexMap = fromList hexDict

toHexMap :: Map Word8 Char
toHexMap = fromList [(snd p, fst p) | p <- hexDict]

fromBase64Map :: Map Char Word8
fromBase64Map = fromList base64Dict

toBase64Map :: Map Word8 Char
toBase64Map = fromList [(snd p, fst p) | p <- base64Dict]

fromHex :: Hex -> B.ByteString
fromHex (Hex s) = B.pack . addPairs . map (fromHexMap !) . T.unpack $ s
    where addPairs (a:b:cs) = 16 * a + b : addPairs cs
          addPairs []       = []
          addPairs [a]      = undefined

toHex :: B.ByteString -> Hex
toHex = Hex . T.pack . map (toHexMap !) . splitToPairs . B.unpack
    where splitToPairs (w:ws) = w `div` 16 : w `mod` 16 : splitToPairs ws
          splitToPairs []     = []

fromBase64 :: Base64 -> B.ByteString
fromBase64 (Base64 t) = B.pack . concat . fmap convert . chunksOf 4 . T.unpack $ t
    where convert (a:b:'=':'=':[]) = return $ val1 a b
          convert (a:b:c  :'=':[]) = [ val1 a b
                                     , val2 b c ]
          convert (a:b:c  :d  :[]) = [ val1 a b
                                     , val2 b c
                                     , val3 c d ]
          convert xs               = undefined
          val1 a b = 4 * (fromBase64Map ! a) + (fromBase64Map ! b) `div` 16
          val2 b c = 16 * ((fromBase64Map ! b) `mod` 16) + (fromBase64Map ! c) `div` 4
          val3 c d = 64 * ((fromBase64Map ! c) `mod` 4)  + (fromBase64Map ! d)

toBase64 :: B.ByteString -> Base64
toBase64 = Base64 . T.pack . concat . fmap convert . chunksOf 3 . B.unpack
    where convert [a]       = [ toBase64Map ! val1 a
                              , toBase64Map ! val2 a 0
                              , '='
                              , '=' ]
          convert [a, b]    = [ toBase64Map ! val1 a
                              , toBase64Map ! val2 a b
                              , toBase64Map ! val3 b 0
                              , '=' ]
          convert [a, b, c] = [ toBase64Map ! val1 a
                              , toBase64Map ! val2 a b
                              , toBase64Map ! val3 b c
                              , toBase64Map ! val4 c ]
          val1 a   = a `div` 4
          val2 a b = 16 * (a `mod` 4) + (b `div` 16)
          val3 b c = 4 * (b `mod` 16) + (c `div` 64)
          val4 c   = c `mod` 64

