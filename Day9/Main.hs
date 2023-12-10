module Main (main) where

import Text.Parsec
import Data.Functor (($>))
import Control.Applicative (liftA2)

inputP :: Parsec String () [[Int]]
inputP = manyTill ((num `sepBy1` char ' ')<*newline) eof
    where num = liftA2 (*)
                   (option 1 (char '-'$>(-1)))
                   (read <$> many1 digit)

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
    let sequences = either (error . show) id $
                        parse inputP "" input
        solutions = sum . flip map sequences <$> [extrap, extrap'] 
    mapM_ print solutions
