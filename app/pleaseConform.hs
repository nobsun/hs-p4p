module Main where

import YouWillAllConform

main :: IO ()
main = interact (unlines . pleaseConform)
