module Main (main) where

import Data.Char
import Data.List
import Text.Parsec

hashAlgo :: String -> Int
hashAlgo = foldl' acc 0 
    where acc i c = ((i + ord c) * 17) `mod` 256

parseFile :: String -> IO [String]
parseFile fName = either (error . show) id . parse inputP "" <$> readFile fName
    where inputP = many1 (noneOf ",\n") `sepBy` char ','

main :: IO ()
main = do
    input <- parseFile "input.txt"

    print $ sum $ hashAlgo <$> input

