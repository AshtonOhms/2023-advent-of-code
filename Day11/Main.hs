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

    print $ sum $ map (uncurry dist) (pairs galaxies)
