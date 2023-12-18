module Main (main) where

import Control.Monad.State
import Data.List
import Data.Matrix
import Data.Set (Set)
import qualified Data.Set as Set

data Dir = N | E | S | W deriving (Show, Eq, Ord)

beamStep :: (Int, Int) -> Char -> Dir -> [((Int, Int), Dir)]
beamStep loc c dir
    | c == '.' = [go loc dir]
    | c == '\\' || c == '/' = [go loc (mir c dir)]
    | c == '|' || c == '-' = go loc <$> spl c dir
    | otherwise = error "unknown char"
    where go (row, col) N = ((row-1, col), N)
          go (row, col) S = ((row+1, col), S)
          go (row, col) W = ((row, col-1), W)
          go (row, col) E = ((row, col+1), E)
          mir '/' N = E
          mir '/' E = N
          mir '/' S = W
          mir '/' W = S
          mir '\\' N = W
          mir '\\' W = N
          mir '\\' S = E
          mir '\\' E = S
          mir _ _ = error "unknown char"
          spl '|' dir' 
            | dir' `elem` [N, S] = [dir]
            | dir' `elem` [E, W] = [N, S]
          spl '-' dir'
            | dir' `elem` [E, W] = [dir]
            | dir' `elem` [N, S] = [E, W]
          spl _ _ = error "unknown char"

beamLocs :: Matrix Char -> (Int, Int) -> Dir -> State (Set ((Int, Int), Dir)) (Set (Int, Int))
beamLocs m loc@(row, col) dir = do
        let inBounds = row > 0 && row <= nrows m
                      && col > 0 && col <= ncols m
        seen <- get
        if (loc, dir) `elem` seen || not inBounds
           then pure Set.empty
           else do
               put $ (loc, dir) `Set.insert` seen
               l <- mapM (uncurry $ beamLocs m) $ beamStep loc (m ! loc) dir
               pure $ foldl' Set.union (Set.singleton loc) l

main :: IO ()
main = do
    input <- fromLists . lines <$> readFile "input.txt"
    let locs = Set.map fst $ execState (beamLocs input (1,1) E) Set.empty

    print $ length locs
