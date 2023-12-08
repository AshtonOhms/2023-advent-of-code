module Main (main) where

import Data.List (foldl')
import Text.Parsec

import Debug.Trace
traceShow' a = traceShow a a
traceShow'' s a = trace (s ++ show a) a

ints :: Parsec String () [Int]
ints = manyTill (do
    _ <- spaces
    read <$> many1 digit) $ try newline

int :: Parsec String () Int
int = do
    ds <- manyTill (do
        _ <- spaces
        many1 digit) $ try newline
    pure $ read $ concat ds

inputP1 :: Parsec String () ([Int], [Int])
inputP1 = do
    _ <- string "Time:"
    times <- ints
    _ <- string "Distance:"
    distances <- ints
    
    pure (times, distances)

inputP2 :: Parsec String () (Int, Int)
inputP2 = do
    _ <- string "Time:"
    time <- int
    _ <- string "Distance:"
    distance <- int
    
    pure (time, distance)

-- Use the quadratic formuler to find the
-- roots of the quadratic 7p^2 - tp + r)
-- where t is the total time of the race,
-- and r is the record to beat.
-- The roots will be the upper and lower
-- bounds of the possible solutions.
sols :: Int -> Int -> Int
sols t r = u - l + 1
    where l = max 0 $ ceiling' $ (realToFrac t - sqrtC) / 2
          u = min t $ floor' $ (realToFrac t + sqrtC) / 2
          ceiling' x = if x == fromIntegral (floor x) then ceiling x + 1 else ceiling x
          floor' x = if x == fromIntegral (floor x) then floor x - 1 else floor x
          sqrtC = sqrt $ fromIntegral (t^(2 :: Int) - 4*r)

part1Sol :: [Int] -> [Int] -> Int
part1Sol ts rs = foldl' (*) 1 $ zipWith sols ts rs

main :: IO ()
main = do
    input <- readFile "input.txt"
    --let (times, distances) = 
    --        case parse inputP1 "" input of
    --          Left e -> error $ show e
    --          Right v -> v
    --print $ part1Sol times distances

    let (time, distance) = 
            case parse inputP2 "" input of
              Left e -> error $ show e
              Right v -> v
    print $ sols time distance
