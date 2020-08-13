module Main where

import Lib
import Set1

main :: IO ()
main = if and [challenge1, challenge2]
          then putStrLn "success"
          else putStrLn "fail"
