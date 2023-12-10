module Main (main) where

import Text.Parsec
import Data.Matrix

import Debug.Trace

inputP :: Parsec String () (Matrix Char)
inputP = fromLists <$>
    manyTill (many1 (oneOf "|-LJ7F.S")<*newline) eof

findStart :: Matrix Char -> (Int, Int)
findStart mat = fst $ head $ filter ((=='S') . snd) $ toList $ mapPos (,) mat



data Dir = N | E | S | W deriving Show

furthestDist :: Matrix Char -> (Int, Int) -> Int
furthestDist mat start = furthestDist' (start, E) (start, N) -- directions chosen specifically for the input
    where furthestDist' a@(locA, _) b@(locB, _)
            | locA == locB' || locA' == locB || locA' == locB' = 1
            | otherwise = traceShow (locA, locB) $ 1 + furthestDist' a' b'
                where a'@(locA', _) = go a
                      b'@(locB', _) = go b

          go ((row, col), dir) = 
              case (p, dir) of
                ('S', S) -> ((row + 1, col), S)
                ('S', N) -> ((row - 1, col), N)
                ('S', E) -> ((row, col + 1), E)
                ('S', W) -> ((row, col - 1), W)

                ('|', S) -> ((row + 1, col), S)
                ('|', N) -> ((row - 1, col), N)

                ('-', E) -> ((row, col + 1), E)
                ('-', W) -> ((row, col - 1), W)

                ('L', W) -> ((row - 1, col), N)
                ('L', S) -> ((row, col + 1), E)

                ('J', S) -> ((row, col - 1), W)
                ('J', E) -> ((row - 1, col), N)

                ('7', E) -> ((row + 1, col), S)
                ('7', N) -> ((row, col - 1), W)

                ('F', N) -> ((row, col + 1), E)
                ('F', W) -> ((row + 1, col), S)
                _ -> error $ "IDK" ++ show (row, col, p, dir)
            where p = mat ! (row, col)


main :: IO ()
main = do
    input <- readFile "input.txt"
    let val = either (error . show) id
            $ parse inputP "" input
        start = findStart val

    print $ furthestDist val start
