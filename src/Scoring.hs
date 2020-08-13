{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Scoring
    ( distFromEnglish
    , fromText
    , fromByteString
    ) where

import qualified "containers" Data.Set as S
import "containers" Data.Map (Map, fromList, keys, elems, (!), adjust)
import "bytestring" Data.ByteString (ByteString)
import "text" Data.Text.Encoding (decodeUtf8)
import "text" Data.Text (Text, toLower)
import qualified "text" Data.Text as T (dropWhile, foldl, head, tail)
import qualified "base" Data.Char as C (isLetter, toLower)
import "base" Data.Maybe (fromJust)
import "base" Data.Monoid

data FrequencyMap = FrequencyMap Int (Map Char Double)

instance Semigroup FrequencyMap where
    (<>) (FrequencyMap 0 m1)
         (FrequencyMap 0 m2)
        = FrequencyMap 0 m1
    (<>) (FrequencyMap n1 m1)
         (FrequencyMap n2 m2)
        = FrequencyMap (n1 + n2)
                       (fromList $ zipWith
                                   ($)
                                   [(\k -> (k, (n1' * (m1 ! k) + n2' * (m2 ! k)) / (n1' + n2'))) | _ <- [0..]]
                                   ['a'..'z'])
        where n1' = fromIntegral n1
              n2' = fromIntegral n2

instance Monoid FrequencyMap where
    mempty = FrequencyMap 0 (fromList $ zip ['a'..'z'] [0..])

fromMap :: Int -> Map Char Double -> Maybe FrequencyMap
fromMap n f = if S.fromList (keys f) == S.fromList ['a'..'z']
                 && (sum (elems f) == 1.0 || sum (elems f) == 0.0)
                  then Just $ FrequencyMap n f
                  else Nothing

-- Only valid on upper- or lower-case letters.
initializeAtChar :: Char -> Maybe FrequencyMap
initializeAtChar c = fromMap 1
                   . adjust (1 +) (C.toLower c)
                   . fromList
                   $ zip ['a'..'z'] [0..]

-- Only valid on upper- or lower-case letters.
adjustFrequencies :: FrequencyMap -> Char -> FrequencyMap
adjustFrequencies (FrequencyMap n f) c
  | C.isLetter c && n == 0 = fromJust $ initializeAtChar c
  | C.isLetter c && n /= 0 = FrequencyMap (n + 1) (increment . multiply $ f)
  | otherwise            = undefined
  where increment = adjust (increase +) (C.toLower c)
        increase  = 1 / fromIntegral (n + 1)
        multiply  = fmap (coeff *)
        coeff     = fromIntegral n / fromIntegral (n + 1)

englishFrequencies :: FrequencyMap
englishFrequencies
  = fromJust . fromMap (9223372036854775807 `div` 2) -- very large int
  . fromList $ zip ['a'..'z']
  [ 0.08497
  , 0.01492
  , 0.02202
  , 0.04253
  , 0.11162
  , 0.02228
  , 0.02015
  , 0.06094
  , 0.07546
  , 0.00153
  , 0.01292
  , 0.04025
  , 0.02406
  , 0.06749
  , 0.07507
  , 0.01929
  , 0.00095
  , 0.07587
  , 0.06327
  , 0.09356
  , 0.02758
  , 0.00978
  , 0.02560
  , 0.00150
  , 0.01994
  , 0.00077 ]

compareFrequencies :: FrequencyMap -> FrequencyMap -> Double
compareFrequencies (FrequencyMap _ f1)
                   (FrequencyMap _ f2)
  = sqrt $ sum [((f1 ! k) - (f2 ! k))**2 | k <- ['a'..'z']]

distFromEnglish :: FrequencyMap -> Double
distFromEnglish f = compareFrequencies f englishFrequencies

fromText :: Text -> FrequencyMap
fromText = body . T.dropWhile (not . C.isLetter)
    where body ""        = mempty
          body t         = countLetters .toLower $ t
          countLetters t = T.foldl adjustFrequencies'
                                       (fromJust . initializeAtChar . T.head $ t)
                                       (T.tail t)
          adjustFrequencies' f c
            | C.isLetter c = adjustFrequencies f c
            | otherwise    = f

fromByteString :: ByteString -> FrequencyMap
fromByteString = fromText . decodeUtf8
