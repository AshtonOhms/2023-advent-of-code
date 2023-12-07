module Main (main) where

import Data.Set (Set)
import qualified Data.Set as S
import Text.Parsec

data Card = Card Int (Set Int) [Int]
    deriving Show

-- Int, spaces
intSpace :: Parsec String () Int
intSpace = do
    i <- many1 digit
    _ <- spaces
    pure (read i :: Int)

cardParser :: Parsec String () Card
cardParser = do
    _ <- string "Card"
    _ <- spaces
    cid <- many1 digit
    _ <- string ":"
    _ <- spaces

    wns <- manyTill intSpace (try $ char '|')
    _ <- spaces
    ns <- manyTill intSpace eof

    pure $ Card (read cid) (S.fromList wns) ns
    

parseCard :: String -> Card
parseCard s = case parse cardParser "" s of
                Left e -> error $ "Could not parse " ++ s ++ ": " ++ show e
                Right c -> c

matches :: Card -> Int
matches (Card _ wns ns) = length $ filter (`S.member` wns) ns

score :: Int -> Int
score 0 = 0
score m = 2^(m-1)


totCardCount :: [Int] -> Int
totCardCount = totCardCount' (repeat 1)
    where totCardCount' (count:counts) (cScore:scores) =
            let scoredCounts = replicate cScore count
             in 1 + count * cScore + totCardCount' (addCounts scoredCounts counts) scores
          totCardCount' [] _ = 0
          totCardCount' _ [] = 0
          addCounts xs ys = [x + y | (x, y) <- zip xs ys] 
                          ++ drop (length xs) ys
                          ++ drop (length ys) xs

part2Sol :: [String] -> Int
part2Sol = totCardCount . map (matches . parseCard)

part1Sol :: [String] -> Int
part1Sol = sum . map (score . matches . parseCard)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let iLines = lines input

    print $ part2Sol iLines
