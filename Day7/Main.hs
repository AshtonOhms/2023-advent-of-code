module Main (main) where

import Data.Char (ord, digitToInt)
import Data.Functor (($>))
import Data.List (group, sort, sortBy)
import Text.Parsec

type Card = Int
type Hand = [Card]
type Bet = (Hand, Int)

cardType :: Hand -> Int
cardType cards
        | [(5, _)] <- cAgg = 7  -- Five of a kind
        | [(1, _), (4, _)] <- cAgg = 6 -- Four of a kind
        | [(2, _), (3, _)] <- cAgg = 5 -- Full house
        | [(1, _), (1, _), (3, _)] <- cAgg = 4 -- Three of a kind
        | [(1, _), (2, _), (2, _)] <- cAgg = 3 -- Two pair
        | [(1, _), (1, _), (1, _), (2, _)] <- cAgg = 2 -- One pair
        | [(1, _), (1, _), (1, _), (1, _), (1, _)] <- cAgg = 1 -- High card
        | otherwise = error $ "Invalid card agg: " ++ show cAgg
        
        where agg :: (Ord a) => [a] -> [(Int, a)]
              agg xs = sort [(length x, head x) | x <- group (sort xs)]
              cAgg = agg cards


compareRank :: Bet -> Bet -> Ordering
compareRank (h2, _) (h1, _) = compare (cardType h2, h2)
                                      (cardType h1, h1)

card :: Parsec String () Card
card = try (char 'A' $> 14)
   <|> try (char 'K' $> 13)
   <|> try (char 'Q' $> 12)
   <|> try (char 'J' $> 11)
   <|> try (char 'T' $> 10)
   <|> digitToInt <$> satisfy isCardNum
       where isCardNum c = (fromIntegral (ord c - ord '2') :: Word) <= 7

hand :: Parsec String () Hand
hand = count 5 card

bet :: Parsec String () Bet
bet = do
    h <- hand
    _ <- space
    i <- read <$> many1 digit
    _ <- newline
    pure (h, i)

part1Sol :: [Bet] -> Int
part1Sol bets = sum $ zipWith winnings ranked [1..]
    where ranked = sortBy compareRank bets
          winnings (_, bid) r = bid*r

main :: IO ()
main = do
    input <- readFile "input.txt"
    let res = parse (many1 bet) "" input
        bets = case res of
          Left e -> error $ show e
          Right v -> v

    mapM_ print $ sortBy compareRank bets
    print $ part1Sol bets
    
