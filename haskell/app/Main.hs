module Main where

import Day01
import Day02
import Day03

main :: IO ()
main = do
  putStrLn "Welcome to Advent of Code"
  putStrLn ""
  Day01.solve
  Day02.solve
  Day03.solve
