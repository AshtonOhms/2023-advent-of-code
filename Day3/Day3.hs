module Day3 (main) where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
traceShow' a = traceShow a a

-- Only valid in the context of this problem :)
isSymbol :: Char -> Bool
isSymbol c = (not . isDigit) c && (c /= '.')

coordsMatching :: (Char -> Bool) -> [[Char]] -> Set (Int, Int)
coordsMatching f rows = Set.fromList $ map fst $ filter (f . snd) $ zip coords $ concat rows
    where coords = [ (x, y) | y <- [1..(length rows)], x <- [1..(length $ head rows)] ]

fstOf3 (a, _, _) = a
thrdOf3 (_, _, a) = a

type NumWithCoords = ([Int], Int, Int)

numsWithCoords :: [[Char]] -> [NumWithCoords]
numsWithCoords = numsWithCoords' 1
    where numsWithCoords' _ [] = []
          numsWithCoords' y (row:rows) = rowCoords row ++ numsWithCoords' (y+1) rows
            where rowCoords = reverse . accum [] [] . zip [1..]
                  toNC xAcc numAcc = (xs, y, num)
                      where num = read $ reverse numAcc :: Int
                            xs = reverse xAcc
                  accum :: [Int] -> [Char] -> [(Int, Char)] -> [([Int], Int, Int)]
                  accum xAcc numAcc ((x, c):xcs)
                      | isDigit c = accum (x:xAcc) (c:numAcc) xcs
                      | (not . null) numAcc = toNC xAcc numAcc:accum [] [] xcs
                      | otherwise = accum [] [] xcs
                  accum xAcc numAcc []
                      | (not . null) numAcc = [toNC xAcc numAcc]
                      | otherwise = []

type NumId = Int
numsMap :: [NumWithCoords] -> Map (Int, Int) (NumId, Int)
numsMap ncs = Map.fromList $ concatMap ncp $ zip [1..] ncs
    where ncp (id, (xs, y, n)) = [ ((x, y), (id, n)) | x <- xs ]

findGearRatios :: Map (Int, Int) (NumId, Int) -> [(Int, Int)] -> [Int]
findGearRatios nMap ((gx, gy):gcs)
    | any (`Map.member` nMap) neighbors, [(_,a),(_,b)] <- gearNums = (a*b):findGearRatios nMap gcs
    | otherwise = findGearRatios nMap gcs
    where neighbors = [ (x, y) | y <- [gy-1..gy+1], x <- [gx-1..gx+1] ]
          gearNums = nub $ mapMaybe (`Map.lookup` nMap) neighbors
findGearRatios _ [] = []


findNums :: [([Int], Int, Int)] -> Set (Int, Int) -> [Int]
findNums ncs symCoords = map thrdOf3 $ filter nearSymbol ncs
    where nearSymbol (xs, y, n) = (not . null) 
                                $ Set.intersection symCoords 
                                $ Set.fromList $ [ (x', y') | y'<-[y-1..y+1], x' <- [head xs-1..last xs+1] ]

part1Sol :: [[Char]] -> Int
part1Sol = sum . uncurry findNums . (numsWithCoords &&& coordsMatching isSymbol)


part2Sol :: [[Char]] -> Int
part2Sol = sum . uncurry findGearRatios . ( (numsMap . numsWithCoords) &&& (Set.toList . coordsMatching (=='*')) )

main :: IO ()
main = do
    input <- readFile "input.txt"
    let iLines = lines input

    print $ part2Sol iLines
