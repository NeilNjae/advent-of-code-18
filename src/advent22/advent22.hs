{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import Debug.Trace

-- import Prelude hiding ((++))

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.List

import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S
import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>), (><))
import Data.Foldable (toList, foldr', foldl', all)
import Data.Maybe (fromJust)
import Debug.Trace

type Coord = (Integer, Integer)
type Cave = M.Map Coord Integer


data Region = Rocky | Wet | Narrow deriving (Eq, Ord, Show)
data Tool = Rope | Torch | Neither deriving (Eq, Ord, Show)
data Explorer = Explorer { _tool :: Tool
                         , _coord :: Coord
                         , _time :: Integer
                         } deriving (Ord, Show)
type ExploredStates = S.Set Explorer

type RegionCave = M.Map Coord Region

data Agendum = Agendum { _current :: Explorer
                       , _trail :: Q.Seq Explorer
                       , _cost :: Int} deriving (Show, Eq)
type Agenda = P.MinPQueue Int Agendum 
type Candidates = S.Set (Int, Agendum)


instance Eq Explorer where
    e1 == e2   = (_tool e1 == _tool e2) && (_coord e1 == _coord e2)


depth :: Integer
-- depth = 510
depth = 10689

target :: Coord
-- target = (10, 10)
target = (11, 722)

width :: Integer
width = fst target

height :: Integer
height = snd target


main :: IO ()
main = do 
        print $ part1
        print $ part2
        -- print $ part2 ip instrs

part1 = cave_risk_level $ erosion_levels width height

part2 = _time $ _current $ fromJust result
    where cave = region_cave $ erosion_levels (width + height + 10)  (width + height + 10)
          result = aStar (initAgenda) cave S.empty



geologic_index_mouth = 0
geologic_index_target = 0
geologic_index_y0 x =  x * 16807
geologic_index_x0 y =  y * 48271
geologic_index l u = l * u

erosion_level gi = (gi + depth) `mod` 20183

risk_level el = el `mod` 3

region_type 0 = Rocky
region_type 1 = Wet
region_type 2 = Narrow

erosion_levels :: Integer -> Integer -> Cave
erosion_levels w h = M.insert (width, height) (erosion_level $ geologic_index_target) cave
    where cave0 = M.singleton (0, 0) $ erosion_level $ geologic_index_mouth
          cave_top = foldl' (\c x -> M.insert (x, 0) (erosion_level $ geologic_index_y0 x) c) cave0 [1..w]
          cave_left = foldl' (\c y -> M.insert (0, y) (erosion_level $ geologic_index_x0 y) c) cave_top [1..h]
          cave = foldl' insert_erosion_level 
                        cave_left
                        [ (xx, yy) | xx <- [1..w], yy <- [1..h] ]
          insert_erosion_level c (x, y) = M.insert (x, y) (erosion_level $ geologic_index (c!((x - 1), y)) (c!(x, (y - 1)))) c

cave_risk_level cave = sum $ map risk_level $ M.elems cave

region_cave cave = M.map (region_type . risk_level) cave


initAgenda :: Agenda
initAgenda = P.singleton (estimateCost explorer) Agendum { _current = explorer, _trail = Q.empty, _cost = estimateCost explorer}
    where explorer = Explorer { _coord = (0, 0), _tool = Torch, _time = 0 }


aStar :: Agenda -> RegionCave -> ExploredStates -> Maybe Agendum
-- aStar [] _ = Agendum {current=buildingTest, trail=[], cost=0}
aStar agenda cave closed 
    -- | trace ("Peeping " ++ (show $ fst $ P.findMin agenda) ++ ": " ++ (show reached) ++ " <- " ++ (show $ toList $ Q.take 1 $ _trail $ currentAgendum) ++ " :: " ++ (show newAgenda)) False = undefined
    -- | trace ("Peeping " ++ (show $ P.findMin agenda) ) False = undefined
    | P.null agenda = Nothing
    | otherwise = 
        if isGoal reached then Just currentAgendum
        else if reached `S.member` closed 
            then aStar (P.deleteMin agenda) cave closed
            else aStar newAgenda cave (S.insert reached closed)
        where 
            (_, currentAgendum) = P.findMin agenda
            reached = _current currentAgendum
            newAgenda = foldl' (\q a -> P.insert (_cost a) a q) (P.deleteMin agenda) $ candidates currentAgendum cave closed



candidates :: Agendum -> RegionCave -> ExploredStates -> (Q.Seq Agendum)
candidates agendum cave closed = newCandidates
    where
        candidate = _current agendum
        previous = _trail agendum
        succs = legalSuccessors cave $ successors candidate
        nonloops = Q.filter (\s -> not $ s `S.member` closed) succs
        newCandidates = fmap (\n -> makeAgendum n) nonloops
        makeAgendum new = Agendum { _current = new
                                  , _trail = candidate <| previous
                                  , _cost = estimateCost new + (fromIntegral $ _time new)
                                  }

isGoal :: Explorer -> Bool
isGoal explorer = (_coord explorer) == target && (_tool explorer) == Torch


isLegal :: RegionCave -> Explorer -> Bool
isLegal cave explorer = 
    legalInRegion region tool
    where region = cave!(_coord explorer)
          tool = _tool explorer

legalInRegion :: Region -> Tool -> Bool
legalInRegion Rocky Rope = True
legalInRegion Rocky Torch = True
legalInRegion Wet Rope = True
legalInRegion Wet Neither = True
legalInRegion Narrow Torch = True
legalInRegion Narrow Neither = True
legalInRegion _ _ = False


successors :: Explorer -> (Q.Seq Explorer)
successors explorer = movingSuccessors >< switchingSuccessors
    where time = _time explorer
          (x, y) = _coord explorer
          tool = _tool explorer
          locations = filter (\(x', y') -> x' >= 0 && y' >= 0) 
                            [(x, y + 1),  (x, y - 1), (x + 1, y), (x - 1, y)]
          tools = [t | t <- [Rope, Torch, Neither] , t /= tool ]
          movingSuccessors = fmap (\l -> explorer { _coord = l, _time = time + 1}) $ Q.fromList locations
          switchingSuccessors = fmap (\t -> explorer { _tool = t, _time = time + 7}) $ Q.fromList tools


legalSuccessors :: RegionCave -> (Q.Seq Explorer) -> (Q.Seq Explorer)
legalSuccessors cave = Q.filter (isLegal cave)


estimateCost :: Explorer -> Int
estimateCost explorer = fromIntegral $ (abs (x - width)) + (abs (y - height))
    where (x, y) = _coord explorer


