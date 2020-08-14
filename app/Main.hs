module Main where

import Lib
import Set1

main :: IO ()
main = do
    putStrLn $ if and [challenge1, challenge2]
                  then "success"
                  else "fail"
    print challenge3Secret
