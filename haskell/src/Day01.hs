module Day01 where

import qualified Data.Set as Set

parseSignedNum :: String -> Int
parseSignedNum s =
  let
    sign = head s
    num = read $ tail s
  in
    if sign == '-' then negate num else num

findDuplicateFreq :: [Int] -> Int
findDuplicateFreq freqs = doIt 0 Set.empty (cycle freqs)
  where
    doIt :: Int -> Set.Set Int -> [Int] -> Int
    doIt current previous (f : fs)
      | Set.member current previous = current
      | otherwise = doIt (current + f) (Set.insert current previous) fs


loadInput :: IO [Int]
loadInput = parseInput <$> readFile "../input/day01.txt"
  where
    parseInput = map parseSignedNum . lines

part1 :: IO ()
part1 = do
  freqs <- loadInput
  let
    result = sum freqs
  putStrLn ("The final frequency is " ++ show result)

part2 :: IO ()
part2 = do
  freqs <- loadInput
  let
    firstDuplicate = findDuplicateFreq freqs
  putStrLn ("The first frequency reached twice is " ++ show firstDuplicate)

solve :: IO ()
solve = do
  putStrLn "Day 01"
  part1
  part2
