module Day01 where

parseSignedNum :: String -> Int
parseSignedNum s =
  let
    sign = head s
    num = read $ tail s
  in
    if sign == '-' then negate num else num

parseData :: String -> [Int]
parseData = map parseSignedNum . words
  where
    commaToSpace c = if c == ',' then ' ' else c

findDuplicateFreq :: [Int] -> Int
findDuplicateFreq freqs = doIt 0 [] (cycle freqs)
  where
    doIt current previous (f : fs)
      | current `elem` previous = current
      | otherwise = doIt (current + f) (current : previous) fs


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
  putStrLn ("The first frequency reached twice is" ++ show firstDuplicate)

solve :: IO ()
solve = do
  putStrLn "Day 01"
  part1
  part2
