{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Scoring.Test
    ( scoringTests
    ) where

import "base" Data.Monoid
import "base" Data.Maybe
import "containers" Data.Map ((!))
import qualified "text" Data.Text as T
import "hspec" Test.Hspec
import "QuickCheck" Test.QuickCheck
import "cryptopals" Scoring

initializeAtChar' :: Char -> FrequencyMap
initializeAtChar' = fromJust . initializeAtChar

fromText' :: String -> FrequencyMap
fromText' = fromText . T.filter (`elem` ['a'..'z']) . T.pack

scoringTests :: Spec
scoringTests = do
    testCompareFrequencies
    testInitializeAtChar
    testFromText

testDists = undefined
testFMMonoid = undefined
testAdjustFrequencies = undefined

testInitializeAtChar :: Spec
testInitializeAtChar = do
    describe "initializeAtChar" $ do
        it "returns Just for 'c'" $ do
            isJust (initializeAtChar 'c') `shouldBe` True
        it "returns Nothing for '%'" $ do
            isJust (initializeAtChar '%') `shouldBe` False

testCompareFrequencies :: Spec
testCompareFrequencies = do
    describe "compareFrequencies" $ do
        it "assigns size 1 to unit" $ do
            shouldBe (compareFrequencies (initializeAtChar' 'a')
                                         mempty)
                     1.0
        it "is a proper 2-norm" $ do
            shouldBe (compareFrequencies (fromText' "ab")
                                         (fromText' "bc"))
                     (sqrt 0.5)
        it "assigns distance 0 to identical FMs" $ do
            shouldBe (compareFrequencies (initializeAtChar' 'a')
                                         (initializeAtChar' 'a'))
                     0.0
        it "is symmetric" $ do
            property (\s t -> compareFrequencies (fromText' s) (fromText' t)
                           == compareFrequencies (fromText' t) (fromText' s))

testFromText :: Spec
testFromText = do
    describe "fromText" $ do
        it "gives mempty for \"\"" $ do
            fromText "" `shouldBe` mempty
        it "gives a constant value for one character" $ do
            fromText "c" `shouldBe` initializeAtChar' 'c'
        it "gives frequencies of one-fifth four five different characters" $ do
            shouldBe ((\(FrequencyMap n f) -> f ! 'a') $ fromText "abcde") 0.2

