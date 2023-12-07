module Main (main) where

import Control.Monad (void)
import Data.Either (fromRight)
import Data.List (sort)
import Text.Parsec

import Debug.Trace (trace, traceShow)
traceShow' a = traceShow a a
traceShow'' s a = trace (s ++ show a) a

type DestinationRangeStart = Int
type SourceRangeStart = Int
type RangeLength = Int

data AlmanacMapEntry = 
    AlmanacMapEntry DestinationRangeStart SourceRangeStart RangeLength
    deriving (Show, Eq)

-- Sort first by source range, then by dest range and length
instance Ord AlmanacMapEntry where
    compare (AlmanacMapEntry drsA srsA rlA) 
            (AlmanacMapEntry drsB srsB rlB) = 
                compare (srsA, drsA, rlA)
                        (srsB, drsB, rlB)
    

type AlmanacMap = [AlmanacMapEntry]

data Almanac = Almanac [Int] [AlmanacMap]
    deriving (Show)

intLine :: Parsec String () [Int]
intLine = do
    manyTill (do
        i <- read <$> many1 digit
        _ <- space
        pure i) newline

resource :: Parsec String () String
resource = try (string "seed")
       <|> try (string "soil")
       <|> try (string "fertilizer")
       <|> try (string "water")
       <|> try (string "light")
       <|> try (string "humidity")
       <|> try (string "temperature")
       <|> string "location"

almanacMapEntry :: Parsec String () AlmanacMapEntry
almanacMapEntry = do
    dst <- read <$> many1 digit
    _ <- space
    src <- read <$> many1 digit
    _ <- space
    rng <- read <$> many1 digit
    _ <- newline
    
    pure $ AlmanacMapEntry dst src rng

almanacMap :: Parsec String () AlmanacMap
almanacMap = do
    _ <- resource
    _ <- string "-to-"
    _ <- resource
    _ <- string " map:"
    _ <- newline

    sort <$> manyTill almanacMapEntry (try $ try (void newline) <|> eof)

almanacParser :: Parsec String () Almanac
almanacParser = do
    _ <- string "seeds: "
    seedIds <- intLine

    maps <- count 7 almanacMap

    pure $ Almanac seedIds maps

lookupAlm :: AlmanacMap -> Int -> Int
lookupAlm ((AlmanacMapEntry dst src range):es) i
  | i < src = i
  | i < src + range = dst + (i - src)
  | otherwise = lookupAlm es i
lookupAlm [] i = i

t :: Int -> AlmanacMap -> Int
t x = traceShow'' "  " . (`lookupAlm` x)

seedToLocation :: [AlmanacMap] -> Int -> Int
seedToLocation ms i = foldl t i ms

part1Sol :: Almanac -> Int
part1Sol (Almanac seeds maps) = minimum $ map (traceShow' . seedToLocation maps) seeds

main :: IO ()
main = do
    input <- readFile "input.txt"
    let alm = fromRight (error "Could not parse") $ parse almanacParser "" input
    print $ part1Sol alm
