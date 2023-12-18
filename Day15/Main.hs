module Main (main) where

import Data.Array
import Data.Char
import Data.Functor
import Data.List
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered as OMap
import Text.Parsec

hashAlgo :: String -> Int
hashAlgo = foldl' acc 0 
    where acc i c = ((i + ord c) * 17) `mod` 256

data Op = DashOp String Int | EqOp String Int Int deriving (Show)

type LensBox = OMap String Int
type LensBoxes = Array Int LensBox

parseFile :: String -> IO [Op]
parseFile fName = either (error . show) id . parse inputP "" <$> readFile fName
    where inputP = op `sepBy` char ','
          op = do
              lbl <-  many1 (noneOf ",\n=-")
              let box = hashAlgo lbl
              try (char '-' $> DashOp lbl box)
                <|> char '=' *> (EqOp lbl box . digitToInt <$> digit)

initBoxes :: LensBoxes
initBoxes = listArray (0,255) $ repeat OMap.empty

updateAt :: (Ix i) => i -> (a -> a) -> Array i a -> Array i a
updateAt idx f arr = arr // [(idx, f (arr ! idx))]

step :: LensBoxes -> Op -> LensBoxes
step boxes (DashOp lbl idx) = updateAt idx (OMap.delete lbl) boxes
step boxes (EqOp lbl idx val) = updateAt idx (OMap.alter (const $ Just val) lbl) boxes

focPower :: Int -> LensBox -> Int
focPower boxIdx box = sum $ map ((1+boxIdx)*) 
                    $ zipWith (*) [1..] (snd <$> OMap.assocs box)

main :: IO ()
main = do
    input <- parseFile "input.txt"
    let finalBoxes = foldl' step initBoxes input

    print $ sum $ map (uncurry focPower) (assocs finalBoxes)

