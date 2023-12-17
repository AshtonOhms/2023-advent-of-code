module Main (main) where

import Data.List
import Data.List.Split

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

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    let cols = transpose input
        slidCols = map slideCol cols
        slid = transpose slidCols

    mapM_ putStrLn slid

    print $ sum $ map (scoreCol . slideCol) cols
