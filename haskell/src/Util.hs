module Util where

import Data.Char as C

parseInts :: String -> [Int]
parseInts = go Nothing []
  where
    go (Just i) is [] = reverse (i : is)
    go Nothing is [] = reverse is
    go Nothing is (c:cs) | C.isDigit c = go (Just $ digitToInt c) is cs
    go (Just i) is (c: cs) | C.isDigit c = go (Just $ i * 10 + digitToInt c) is cs
    go (Just i) is (c : cs) = go Nothing (i : is) cs
    go Nothing  is (c : cs) = go Nothing is cs
