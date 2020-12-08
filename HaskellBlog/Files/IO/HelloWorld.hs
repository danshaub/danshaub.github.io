module Main where

main :: IO ()
main = do
  x <- readFile ".\\temp.txt"
  putStrLn x