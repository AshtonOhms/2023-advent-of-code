module Day1 (main, parseDigits) where

import Data.Char (isDigit)
import Debug.Trace
import qualified Data.Set as S

-- Part 1
lineValue :: String -> Int
lineValue line = val
    where digits = filter isDigit line
          val = read [head digits, last digits]

part1Sol :: [String] -> Int
part1Sol input = sum $ map lineValue input

-- Part 2
parseDigits :: String -> String
parseDigits s = parseDigits' s ""
    where parseDigits' [] _ = []
          parseDigits' (c:s) acc 
            | isDigit c = c:parseDigits' s ""
            | curr `elem` [ "o", "on"
                          , "t", "tw"
                          ,      "th", "thr", "thre"
                          , "f", "fo", "fou"
                          ,      "fi", "fiv"
                          , "s", "si"
                          ,      "se", "sev", "seve"
                          , "e", "ei", "eig", "eigh"
                          , "n", "ni", "nin"
                          ] = parseDigits' s curr
            | curr == "one" = '1':continueOv
            | curr == "two" = '2':continueOv
            | curr == "three" = '3':continueOv
            | curr == "four" = '4':continueOv
            | curr == "five" = '5':continueOv
            | curr == "six" = '6':continueOv
            | curr == "seven" = '7':continueOv
            | curr == "eight" = '8':continueOv
            | curr == "nine" = '9':continueOv
            | otherwise = parseDigits' s no1st
            where curr = acc ++ [c]
                  continueOv = parseDigits' (c:s) ""  -- Include current c in next recursion to catch overlap ("twone")
                  _:no1st = curr  -- Remove the 1st character for backtracking -e.g. ("fone" initially gets
                                  -- accumulated as "fo"... but when we encounter "n", the next iteration will check "on")

lineValueP2 :: String -> Int
lineValueP2 line = val
    where digits = parseDigits line 
          val = read [head digits, last digits]
          

part2Sol :: [String] -> Int
part2Sol input = sum $ map lineValueP2 input


main :: IO ()
main = do
    input <- readFile "input.txt"
    let iLines = filter (not . null) $ lines input
    print (part2Sol iLines)
