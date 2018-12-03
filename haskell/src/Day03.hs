{-# LANGUAGE TupleSections #-}
module Day03 where

import Data.Array
import qualified Data.Ix as Ix
import Data.Set (Set)
import qualified Data.Set as Set

import Util

data Patch = Patch { _id:: Int
                   , _x:: Int
                   , _y:: Int
                   , _width:: Int
                   , _height:: Int
                   } deriving (Show, Eq)

type Fabric = Array (Int, Int) (Set Int)
type Cloth = Fabric

parseInput :: String -> [Patch]
parseInput input =
  let
    createPatch (pid : x : y : w : h : _) = Patch pid x y w h
  in
    map (createPatch . parseInts) (lines input)

createCloth :: Cloth
createCloth =
  let
    fabric = listArray ((0,0), (1000, 1000)) (repeat Set.empty)
  in
   fabric

paintPatch :: Cloth -> Patch -> Cloth
paintPatch fabric (Patch pid x y w h) =
  let
    paintCell :: Set Int -> Int -> Set Int
    paintCell = flip Set.insert
    patchCells :: [((Int, Int), Int)]
    patchCells = map (, pid) $ Ix.range ((x, y), (x + w - 1, y + h - 1))
  in
    accum paintCell fabric patchCells

countMultiPatchCells :: Cloth -> Int
countMultiPatchCells fabric =
  let
    cells = elems fabric
    multiPatchCells = filter ((> 1) . length) cells
  in
    length multiPatchCells

part1 :: IO ()
part1 = do
  input <- readFile "../input/day03.txt"
  let patches = parseInput input
  let cloth = foldl paintPatch createCloth patches
  let multis = countMultiPatchCells cloth
  putStrLn ("There are " ++ show multis ++ " cells with more than one patch")

-- part1 :: IO ()
-- part1 = do
--   cloth <- createCloth
--   let patch = Patch 15 2 3 4 5
--   paintPatch cloth patch
--   print 5

solve :: IO ()
solve = do
  putStrLn ""
  putStrLn "Day 3"
  part1
