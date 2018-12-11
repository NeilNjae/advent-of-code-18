{-# LANGUAGE OverloadedStrings #-}

import Data.List
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
-- import qualified Data.Set as S
-- import Data.Function (on)
import Data.Ord (comparing)

type Coord = (Integer, Integer) -- x, y
type Grid = M.Map Coord Integer

key = 5719
-- key = 42

main :: IO ()
main = do 
        let g = makeGrid key
        print $ part1 g
        print $ part2 g


part1 grid = keyOfMaxValue sg
    where sg = allSubCellPower 3 grid


part2 grid = maximumBy (comparing snd) $ [bestInGrid size grid | size <- [3..300]]

-- bestSubCell size grid = 

makeGrid :: Integer -> Grid
makeGrid key = M.fromList [((x, y), powerLevel x y key) | x <- [1..300], y <- [1..300] ]

powerLevel :: Integer -> Integer -> Integer -> Integer
powerLevel x y key = ((interim `div` 100) `mod` 10) - 5
    where rackID = x + 10
          interim = ((rackID) * y + key) * rackID

subCellPower :: Integer -> Integer -> Integer -> Grid -> Integer
subCellPower size x y grid = sum [grid!(sx, sy) | sx <- [x..(x+size-1)], sy <- [y..(y+size-1)]]

allSubCellPower :: Integer -> Grid -> Grid
allSubCellPower size grid = M.fromList [((x, y), subCellPower size x y grid)| x <- [1..(301-size)], y <- [1..(301-size)]]


keyAndMaxValue :: Ord b => M.Map a b -> (a, b)
keyAndMaxValue m = M.foldrWithKey mergeKV (M.findMin m) m
    where mergeKV k v (bestK, bestV) = 
            if v > bestV then (k, v) else (bestK, bestV)

keyOfMaxValue :: Ord b => M.Map a b -> a
keyOfMaxValue m = fst $ keyAndMaxValue m

bestInGrid :: Integer -> Grid -> ((Integer, Integer, Integer), Integer)
bestInGrid size grid = ((x, y, size), p)
    where ((x, y), p) = keyAndMaxValue $ allSubCellPower size grid
