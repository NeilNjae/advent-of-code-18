{-# LANGUAGE OverloadedStrings #-}


import Prelude hiding (Left, Right)
import Data.List
import Data.Tuple (swap)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Debug.Trace

type Coord = (Int, Int) -- x, y
data Cell = Horizontal | Vertical | TopLeft | TopRight | Junction deriving (Show, Eq)
data Direction = Up | Right | Down | Left deriving (Show, Eq, Ord, Enum, Bounded)
data Decision = Anticlockwise | Straight | Clockwise deriving (Show, Eq, Ord, Enum, Bounded)
data Cart = Cart Direction Decision deriving (Eq, Show)
type Layout = M.Map Coord Cell
type Carts = M.Map Coord Cart


main :: IO ()
main = do 
    text <- readFile "data/advent13.txt"
    let (layout, carts) = parse text
    -- print carts
    -- print layout
    -- print $ propogateUntilCollision (orderedCarts carts) layout carts
    putStrLn $ showCoord $ part1 carts layout
    putStrLn $ showCoord $ part2 carts layout


part1 :: Carts -> Layout -> Coord
part1 carts layout = collisionSite
    where (collisionSite, _, _, _) = propogateUntilCollision (orderedCarts carts) layout carts

part2 :: Carts -> Layout -> Coord
part2 carts layout = propogateUntilOne (orderedCarts carts) layout carts

showCoord :: Coord -> String
showCoord (x, y) = show x ++ "," ++ show y


-- Parsing
parse :: String -> (Layout, Carts)
parse text = foldl' parseRow (M.empty, M.empty) $ zip [0..] (lines text)

parseRow (layout, carts) (y, row) = foldl' parseCellWithY (layout, carts) $ zip [0..] row
    where parseCellWithY = parseCell y

parseCell y (layout, carts) (x, cell) = 
    let here = (x, y)
    in case cell of 
            '-'  -> (M.insert here Horizontal layout, carts)
            '|'  -> (M.insert here Vertical layout, carts)
            '\\' -> (M.insert here TopLeft layout, carts)
            '/'  -> (M.insert here TopRight layout, carts)
            '+'  -> (M.insert here Junction layout, carts)
            '^'  -> (M.insert here Vertical layout, M.insert here (Cart Up Anticlockwise) carts)
            'v'  -> (M.insert here Vertical layout, M.insert here (Cart Down Anticlockwise) carts)
            '<'  -> (M.insert here Horizontal layout, M.insert here (Cart Left Anticlockwise) carts)
            '>'  -> (M.insert here Horizontal layout, M.insert here (Cart Right Anticlockwise) carts)
            _    -> (layout, carts)


-- Moving

-- first argument is the carts left to move this tick.
propogateUntilCollision :: [Coord] -> Layout -> Carts -> (Coord, Coord, [Coord], Carts)
-- propogateUntilCollision cs _ carts | trace ("pUC " ++ show cs ++ " " ++ show carts) False = undefined
-- finished this tick
propogateUntilCollision [] layout carts = 
    if M.size carts <= 1
    then (survivingCoord, survivingCoord, [], carts) -- empty coords list asserts finished a tick with only one cart remaining.
    else propogateUntilCollision (orderedCarts carts) layout carts -- start the next tick
    where survivingCoord = head $ M.keys carts
-- not finished this tick, so move this cart then move the rest
propogateUntilCollision (c:cs) layout carts = 
    if c' `M.member` carts
    then (c', c, cs, carts)
    else propogateUntilCollision cs layout carts'
    where cart = carts!c
          (c', cart') = moveOnce c cart layout
          carts' = M.insert c' cart' $ M.delete c carts

orderedCarts :: Carts -> [Coord]
orderedCarts carts = sortOn swap $ M.keys carts

-- move a cart, without getting as far as collision detection
moveOnce :: Coord -> Cart -> Layout -> (Coord, Cart)
moveOnce coord (Cart direction decision) layout = (coord', (Cart direction'' decision'))
    where coord' = takeStep direction coord
          direction' = (curve direction (layout!coord'))
          (direction'', decision') = junctionTurn (layout!coord') direction' decision

-- keep moving carts until only one left
-- move carts until there's a collision; remove those carts then carry on
propogateUntilOne :: [Coord] -> Layout -> Carts -> Coord
propogateUntilOne coords layout carts = 
    if null coords' -- only when finished a tick and only one cart remaining.
    then c1
    else propogateUntilOne coords'' layout carts''
    where (c1, c2, coords', carts') = propogateUntilCollision coords layout carts
          carts'' = M.delete c1 $ M.delete c2 carts'
          coords'' = filter (/= c1) $ filter (/= c2) coords'

-- Move in the current direction
takeStep :: Direction -> Coord -> Coord
takeStep Up (x, y) = (x, y-1)
takeStep Down (x, y) = (x, y+1)
takeStep Left (x, y) = ( x-1, y)
takeStep Right (x, y) = (x+1, y)

curve :: Direction -> Cell -> Direction 
curve Up TopLeft = Left
curve Down TopLeft = Right
curve Left TopLeft = Up
curve Right TopLeft = Down
curve Up TopRight = Right
curve Down TopRight = Left
curve Left TopRight = Down
curve Right TopRight = Up
curve d _ = d

junctionTurn :: Cell -> Direction -> Decision -> (Direction, Decision)
junctionTurn Junction direction Anticlockwise = (predW direction, Straight)
junctionTurn Junction direction Straight      = (direction,       Clockwise)
junctionTurn Junction direction Clockwise     = (succW direction, Anticlockwise)
junctionTurn _        direction decision      = (direction, decision)


-- | a `succ` that wraps 
succW :: (Bounded a, Enum a, Eq a) => a -> a 
succW dir | dir == maxBound = minBound
          | otherwise = succ dir

-- | a `pred` that wraps
predW :: (Bounded a, Enum a, Eq a) => a -> a
predW dir | dir == minBound = maxBound
          | otherwise = pred dir

