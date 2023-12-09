module Main (main) where

import Text.Parsec

inputP :: Parsec String () [[Int]]
inputP = manyTill ((num `sepBy1` char ' ')<*newline) eof
    where num = do
            neg <- optionMaybe $ char '-'
            n <- many1 digit
            pure $ maybe 1 (const (-1)) neg * read n

diffs :: [Int] -> [Int]
diffs (a:b:ns) = b-a:diffs (b:ns)
diffs [_] = []
diffs [] = []

extrap :: [Int] -> Int
extrap ns 
  | all (==0) ns = 0
  | otherwise = last ns + extrap (diffs ns)

extrap' :: [Int] -> Int
extrap' ns
  | all (==0) ns = 0
  | otherwise = head ns - extrap' (diffs ns)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let sequences = case parse inputP "" input of
                      Left e -> error $ show e
                      Right v -> v

    print $ sum $ map extrap sequences
    print $ sum $ map extrap' sequences
