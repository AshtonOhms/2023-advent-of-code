module Main (main) where

import Data.Foldable
import Data.List
import Data.List.Split
import qualified Data.Set.Ordered as OSet

compareRock :: Char -> Char -> Ordering
compareRock 'O' '.' = LT
compareRock '.' 'O' = GT
compareRock a b = compare a b

slideCol :: [Char] -> [Char]
slideCol cs = intercalate "#" 
              $ map (sortBy compareRock) 
              $ splitOn "#" cs

scoreCol :: [Char] -> Int
scoreCol = sum . zipWith (\s c -> if c=='O' then s else 0) [1..] . reverse

data Dir = N | W | S | E deriving (Show, Eq, Ord)

slide :: Dir -> [[Char]] -> [[Char]]
slide W = map slideCol 
slide E = map (reverse . slideCol . reverse)
slide N = transpose . map slideCol . transpose
slide S = transpose . map (reverse . slideCol . reverse) . transpose

load :: [[Char]] -> Int
load = sum . map scoreCol . transpose

-- finds the start and length of cycle in the sequence
-- returns (# states before cycle, # states in cycle, [states])
findCycle :: [[Char]] -> (Int, Int, [[[Char]]])
findCycle = findCycle' OSet.empty
    where findCycle' seen pattern
            | Just i <- pattern `OSet.findIndex` seen = 
                (i, length seen - i, drop i $ toList seen)
            | otherwise = findCycle'
                          (seen OSet.|> pattern)
                          (foldl' (flip slide) pattern [N,W,S,E])

patternAtCycle :: [[Char]] -> Int -> [[Char]]
patternAtCycle pattern cycles = states !! cycles'
    where (preCycle, cycleLength, states) = findCycle pattern
          cycles' = (cycles - preCycle) `mod` cycleLength

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print $ load $ patternAtCycle input 1000000000
