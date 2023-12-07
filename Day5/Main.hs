module Main (main) where

import Control.Monad (void)
import Data.Either (fromRight)
import Data.List (sortBy)
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

compareBySource :: AlmanacMapEntry -> AlmanacMapEntry -> Ordering
compareBySource (AlmanacMapEntry drsA srsA rlA) 
            (AlmanacMapEntry drsB srsB rlB) = 
                compare (srsA, drsA, rlA)
                        (srsB, drsB, rlB)
    
compareByDest :: AlmanacMapEntry -> AlmanacMapEntry -> Ordering
compareByDest (AlmanacMapEntry drsA srsA rlA) 
            (AlmanacMapEntry drsB srsB rlB) = 
                compare (drsA, srsA, rlA)
                        (drsB, srsB, rlB)

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

    sortBy compareBySource <$> manyTill almanacMapEntry (try $ try (void newline) <|> eof)

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

seedToLocation :: [AlmanacMap] -> Int -> Int
seedToLocation ms i = foldl (flip lookupAlm) i ms

part1Sol :: Almanac -> Int
part1Sol (Almanac seeds maps) = minimum $ map (seedToLocation maps) seeds

seedToSeeds :: [Int] -> [Int]
seedToSeeds (start:range:is) = take range [start..] ++ seedToSeeds is
seedToSeeds [_] = error "Odd number of seeds!"
seedToSeeds [] = []

part2Sol :: Almanac -> Int
part2Sol (Almanac seeds maps) = minimum $ map (seedToLocation maps) (seedToSeeds seeds)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let alm = fromRight (error "Could not parse") $ parse almanacParser "" input
    print $ part2Sol alm
