module Main where

import qualified MyLib (someFunc, bar)

foo x = x + 1

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  print $ foo $ MyLib.bar "abc"
  print $ foo $ MyLib.bar "abcd"
  MyLib.someFunc
  MyLib.someFunc
  MyLib.someFunc
