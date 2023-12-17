module Main where

import Control.Monad (void)
import Data.Either
import Data.List (transpose)
import Text.Parsec

import Debug.Trace

inputP :: Parsec String () [[[Char]]]
inputP = many1 $ many1 row<*try end
    where end = void (try newline) <|> eof
          row = many1 (oneOf ".#")<*newline

findReflect :: [[Char]] -> Maybe Int
findReflect (r:rs) = findReflect' [r] rs
    where findReflect' acc (c:cs)
            | startEq acc (c:cs) = Just $ length acc
            | otherwise = traceShow (acc, c:cs) findReflect' (c:acc) cs
          findReflect' _ _ = Nothing
          startEq (a:as) (b:bs)
            | a /= b = False
            | a == b = startEq as bs
          startEq [a] [b] = a == b
          startEq _ _ = True

findReflects :: [[Char]] -> Either Int Int
findReflects p
    | Just i <- findReflect p = Left i 
    | Just i <- findReflect (transpose p) = Right i
    | otherwise = error "no reflection"

sol :: [Either Int Int] -> Int
sol es = sum vert * 100 + sum horiz
    where horiz = fromRight 0 <$> filter isRight es
          vert = fromLeft 0 <$> filter isLeft es

main :: IO ()
main = do
    input <- readFile "input.txt"
    let patterns = either (error . show) id $ parse inputP "" input

    print $ sol $ map findReflects patterns
