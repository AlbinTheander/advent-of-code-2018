module Day02 where

import Data.List

readInput :: IO [String]
readInput = lines <$> readFile "../input/day02.txt"


hasGroupWithExactly :: Int -> String -> Bool
hasGroupWithExactly n xs =
  let
    charGroups = group $ sort xs
  in any ((== n) . length) charGroups

areClose :: String -> String -> Bool
areClose s1 s2 =
  let
    matchingChars = filter (uncurry (==)) (zip s1 s2)
  in
    length matchingChars == length s1 - 1

part1 :: IO ()
part1 = do
  input <- readInput
  let doubles = filter (hasGroupWithExactly 2) input
  let triples = filter (hasGroupWithExactly 3) input
  let result = length doubles * length triples
  putStrLn ("The checksum is " ++ show result)

part2 :: IO ()
part2 = do
  input <- readInput
  let pairs = [(s1, s2) | s1 <- input, s2 <- input, s1 < s2]
  let (s1, s2) = head $ filter (uncurry areClose) pairs
  let commonChars = map fst $ filter (uncurry (==)) (zip s1  s2)
  putStrLn ("The common letters between the box ID:s are " ++ commonChars)

solve :: IO ()
solve = do
  putStrLn ""
  putStrLn "Day 2"
  part1
  part2
