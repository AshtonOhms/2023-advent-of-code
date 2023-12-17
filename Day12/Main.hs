module Main where

import Control.Monad.Memo
import Data.List (intercalate)
import Text.Parsec

inputP :: Parsec String () [([Char], [Int])]
inputP = many1 $ (,)
    <$> many1 (oneOf ".#?")<*char ' '
    <*> ((read <$> many1 digit) `sepBy` char ',')<* newline


possib :: [Char] -> [Int] -> Memo (Int, [Char], [Int]) Int Int
possib = possib' 0
    where possib' :: Int -> [Char] -> [Int] -> Memo (Int, [Char], [Int]) Int Int
          possib' run (c:cs) (g:gs)
                | run > g = return 0
                | c == '.' = if run == 0 then for3 memo possib' 0 cs (g:gs)
                             else if run == g then for3 memo possib' 0 cs gs
                             else return 0

                | c == '#' = possib' (run+1) cs (g:gs)

                | c == '?' = do
                              a <- for3 memo possib' run ('#':cs) (g:gs)
                              b <- for3 memo possib' run ('.':cs) (g:gs)
                              return $ a+b

                | otherwise = error "unknown char"

          possib' run [] [g] = return $ if run==g then 1 else 0
          possib' _ [] [] = return 1
          possib' _ cs [] = return $ if all (\c -> c =='.' || c == '?') cs then 1 else 0 
          possib' _ [] _ = return 0

unfold :: ([Char], [Int]) -> ([Char], [Int])
unfold (cs, gs) = ( intercalate "?" (replicate 5 cs)
                  , concat (replicate 5 gs))

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rows = either (error . show) id (parse inputP "" input)

    -- Part 1
    --print $ sum $ map (startEvalMemo . uncurry possib) rows
    
    -- Part 2
    print $ sum $ map (startEvalMemo . uncurry possib . unfold) rows
