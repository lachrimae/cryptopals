{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Set1
    ( challenge1
    , challenge2
    , challenge3Secret
    ) where

import "text" Data.Text.Encoding (decodeUtf8)
import "text" Data.Text (Text, toUpper, pack)
import "base" Data.Maybe (isJust, catMaybes, fromJust)
import "base" Data.List (sortBy)
import "base" GHC.Exts (toList)
import Encoding
import Bitwise
import Scoring

-- Challenge 1
challenge1Output :: Base64
challenge1Output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

challenge1Input :: Hex
challenge1Input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge1 :: Bool
challenge1 = challenge1Output == (toBase64 $ fromHex challenge1Input)

-- Challenge 2
challenge2Output :: Hex
challenge2Output = "746865206b696420646f6e277420706c6179"

challenge2Input1 :: Hex
challenge2Input1 = "1c0111001f010100061a024b53535009181c"

challenge2Input2 :: Hex
challenge2Input2 = "686974207468652062756c6c277320657965"

challenge2 :: Bool
challenge2 = if isJust output
                then challenge2Output == (toHex $ fromJust output)
                else False
            where output = xor (fromHex challenge2Input1) (fromHex challenge2Input2)

-- Challenge 3
challenge3Input :: Hex
challenge3Input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

challenge3Secret  :: Text
challenge3Secret = decodeUtf8 . head . sortBy (\a b -> compare (dfe a) (dfe b)) . catMaybes
                 $ [xor (fromHex . constHex $ c) (fromHex challenge3Input) | c <- ['0'..'9'] ++ ['a'..'f']]
                     where constHex  = Hex . pack . replicate length'
                           dfe       = distFromEnglish . fromByteString
                           length'   = length . toList $ challenge3Input
