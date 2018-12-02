module Main where

import Day01
import Day02

main :: IO ()
main = do
  putStrLn "Welcome to Advent of Code"
  putStrLn ""
  Day01.solve
  Day02.solve
