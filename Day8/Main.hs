module Main (main) where

import Data.Functor (($>))
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
    src <- count 3 letter
    _ <- string " = ("
    dst1 <- count 3 letter
    _ <- string ", "
    dst2 <- count 3 letter
    _ <- char ')'
    _ <- newline
    pure (src, (dst1, dst2))

inputP :: Parsec String () ([Dir], Network)
inputP = do
    dirs <- many1 dir
    _ <- count 2 newline
    es <- many1 entry
    pure (dirs, Map.fromList es)

travelToZZZ :: Network -> [Dir] -> String -> [String]
travelToZZZ _ _ "ZZZ" = []
travelToZZZ n (d:ds) node = node:travelToZZZ n ds (
        case d of
          L -> lNode
          R -> rNode)
    where (lNode, rNode) = n Map.! node
travelToZZZ _ [] _ = error "no more directions!"

main :: IO ()
main = do
    input <- readFile "input.txt"
    
    let (dirs, network) =
            case parse inputP "" input of
              Left e -> error $ show e
              Right v -> v

    print $ length $ travelToZZZ network (cycle dirs) "AAA"
