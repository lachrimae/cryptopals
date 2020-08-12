{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Set1
    ( challenge1
    ) where

import "text" Data.Text (toUpper)
import Encoding

challenge1Output :: Base64
challenge1Output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

challenge1Input :: Hex
challenge1Input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge1 :: Bool
challenge1 = challenge1Output == (toBase64 $ fromHex challenge1Input)
