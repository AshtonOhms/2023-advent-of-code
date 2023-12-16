module Main where

import Data.Foldable
import Text.Parsec

import Debug.Trace

--traceShow b a = a

inputP :: Parsec String () [([Char], [Int])]
inputP = many1 $ (,) 
    <$> many1 (oneOf ".#?")<*char ' '
    <*> ((read <$> many1 digit) `sepBy` char ',')<* newline


possib :: [Char] -> [Int] -> Int
possib = possib' 0
    where possib' :: Int -> [Char] -> [Int] -> Int
          possib' run (c:cs) (g:gs)
                | run > g = 0
                | c == '.' = traceShow (run, c:cs, g:gs) $ 
                                if run == 0 then possib' 0 cs (g:gs)
                                else if run == g then possib' 0 cs gs
                                else traceShow "wrong run size" 0

                | c == '#' = traceShow (run, c:cs, g:gs) $ 
                                possib' (run+1) cs (g:gs)

                | c == '?' = traceShow (run, c:cs, g:gs) 
                                (possib' run ('#':cs) (g:gs)
                               + possib' run ('.':cs) (g:gs))

                | otherwise = error "unknown char"

          possib' run [] [g] = traceShow (run, [] :: [Int], [g]) $
                                if run==g then 1 else 0
          possib' _ [] [] = traceShow "valid!" 1
          possib' _ cs [] = traceShow "nums empty" $
              if all (\c -> c =='.' || c == '?') cs then 1 else 0 
          possib' _ [] _ = traceShow "cs empty" 0

main :: IO ()
main = do
    input <- readFile "input.txt"
    let rows = either (error . show) id (parse inputP "" input)
    --print $ uncurry possib $ last rows -- !! 3
    --for_ rows $ \r -> do
    --    print $ uncurry possib r
    print $ sum $ map (uncurry possib) rows
