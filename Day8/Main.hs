module Main (main) where

import Data.Functor (($>))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec

data Dir = L | R
type Network = Map String (String, String)

dir :: Parsec String () Dir
dir = try (char 'L' $> L)
   <|> (char 'R' $> R)

entry :: Parsec String () (String, (String, String))
entry = do
    src <- count 3 (try letter <|> digit)
    _ <- string " = ("
    dst1 <- count 3 (try letter <|> digit)
    _ <- string ", "
    dst2 <- count 3 (try letter <|> digit)
    _ <- char ')'
    _ <- newline
    pure (src, (dst1, dst2))

inputP :: Parsec String () ([Dir], Network)
inputP = do
    dirs <- many1 dir
    _ <- count 2 newline
    es <- many1 entry
    pure (dirs, Map.fromList es)

travelTo :: (String -> Bool) -> Network -> [Dir] -> String -> [String]
travelTo f n (d:ds) node 
  | f node = []
  | otherwise = node:travelTo f n ds (
        case d of
          L -> lNode
          R -> rNode)
    where (lNode, rNode) = n Map.! node
travelTo _ _ [] _ = error "no more directions!"

main :: IO ()
main = do
    input <- readFile "input.txt"
    
    let (dirs, network) =
            case parse inputP "" input of
              Left e -> error $ show e
              Right v -> v

        startNodes = filter (\node -> last node == 'A') $ Map.keys network

    let pathLengths = map (length . travelTo (\s -> last s=='Z') network (cycle dirs)) startNodes
    print $ foldl' lcm 1 pathLengths
