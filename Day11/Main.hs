{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (transpose)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mat

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (y2-y1) + abs (x2-x1)

pairs :: [a] -> [(a, a)]
pairs (a:as) = map (a,) as ++ pairs as
pairs [] = []

coord :: Int -> [[Char]] -> [Int]
coord idx (row:rows) = let idx' = if all (=='.') row
                                     then idx + 1000000
                                     else idx + 1
                          in idx':coord idx' rows
coord _ [] = []

expand'' :: [[Char]] -> [(Int, Int)]
expand'' m = snd <$> filter ((=='#') . fst) (zip (concat m) [(rowI, colI) | rowI <- rowCs, colI <- colCs ])
    where rowCs = coord 1 m
          colCs = coord 1 $ transpose m
          

expand :: [[Char]] -> [[Char]]
expand (row:rows) = if all (=='.') row
                     then row:row:expand rows
                     else row:expand rows
expand [] = []

mi :: [[Char]] -> Matrix Char
mi = Mat.fromLists . transpose . expand . transpose . expand

main :: IO ()
main = do
    input <- readFile "input.txt"
    
    let m :: Matrix Char
        m = mi (lines input)

        galaxies = fst <$> filter ((=='#') . snd) (Mat.toList (Mat.mapPos (,) m))

    --print $ sum $ map (uncurry dist) (pairs galaxies)
    print $ sum $ map (uncurry dist) (pairs (expand'' (lines input)))
