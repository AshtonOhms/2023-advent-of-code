module Main (main) where

import Text.Parsec
import Data.Functor (($>))

inputP :: Parsec String () [[Int]]
inputP = manyTill ((num `sepBy1` char ' ')<*newline) eof
    where num = (*) <$> option 1 (char '-'$>(-1))
                    <*> (read <$> many1 digit)

diffs :: [Int] -> [Int]
diffs (a:b:ns) = b-a:diffs (b:ns)
diffs _ = []

extrap :: ([Int] -> Int -> Int) -> [Int] -> Int
extrap f ns
  | all (==0) ns = 0
  | otherwise = f ns $ extrap f (diffs ns)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let sequences = either (error . show) id $
                        parse inputP "" input
        solutions = sum . flip map sequences . extrap 
                <$> [ (+) . last, (-) . head ]
    mapM_ print solutions
