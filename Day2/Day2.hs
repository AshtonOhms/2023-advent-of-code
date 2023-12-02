{-# LANGUAGE OverloadedStrings #-}

module Day2 (main) where

import Data.List (foldl')
import Data.Text (Text, splitOn, unpack, pack)

type GameId = Int
data Game = Game GameId [(Int, Int, Int)] -- r, g, b
            deriving Show

gId :: Game -> GameId
gId (Game id _) = id

readGame :: Text -> Game
readGame s = Game id draws
    where [labelS, drawsS] = splitOn ": " s
          ["Game", idS] = splitOn " " labelS
          id = read $ unpack idS

          drawSs = splitOn "; " drawsS
          draws = map readDraw drawSs

          readDraw :: Text -> (Int, Int, Int)
          readDraw dS = foldl' (\(r, g, b) cS ->
                 let [valS, color] = splitOn " " cS
                     val = read $ unpack valS
                  in case color of
                    "red" -> (val, g, b)
                    "green" -> (r, val, b)
                    "blue" -> (r, g, val)
                    _ -> error $ "No color " ++ unpack color

            ) (0, 0, 0) (splitOn ", " dS)

gamePossibleWith :: (Int, Int, Int) -> Game -> Bool
gamePossibleWith (r, g, b) (Game _ ds) = all drawPossible ds
    where drawPossible (dr, dg, db) = dr <= r && dg <= g && db <=b

minCubes :: Game -> (Int, Int, Int)
minCubes (Game _ draws) = (mr, mg, mb)
    where mr = maximum $ map r draws
          mg = maximum $ map g draws
          mb = maximum $ map b draws
          r (v, _, _) = v
          g (_, v, _) = v
          b (_, _, v) = v

power :: (Int, Int, Int) -> Int
power (r, g, b) = r*g*b

main :: IO ()
main = do
    input <- readFile "input.txt"
    let iLines = filter (not . null) $ lines input
        games = map (readGame . pack) iLines
    --print $ sum $ map gId $ filter (gamePossibleWith (12, 13, 14)) games
    print $ sum $ map (power . minCubes) games

