{-# LANGUAGE OverloadedStrings #-}

-- import Debug.Trace

import Data.Text (Text)
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List
-- import qualified Data.Set as S

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
-- import Data.Tuple (swap)

type Coord = (Int, Int) -- row, col
data Cell = Open | Trees | Lumberyard deriving (Eq, Enum, Bounded, Ord)
type World = M.Map Coord Cell
type Cache = M.Map World Int

instance Show Cell where
    show Open = "."
    show Trees = "|"
    show Lumberyard = "#"

main :: IO ()
main = do 
    text <- TIO.readFile "data/advent18.txt"
    let worldSpec = successfulParse text
    let world = makeWorld worldSpec
    -- print $ neighbours (1, 1) world
    -- putStrLn $ showWorld world
    -- putStrLn $ showWorld $ generation world
    -- putStrLn $ showWorld $ (iterate generation world)!!10
    print $ part1 world
    print $ part2 world

part1 :: World -> Int
part1 world = score ((iterate generation world)!!10)

part2 :: World -> Int
part2 world = score usedWorld
    where (worlds, repeated) = cacheWorlds world
          lastMinute = M.size worlds
          prevMinute = worlds!repeated
          final = 1000000000
          cycleLength = lastMinute - prevMinute
          nCycles = (final - lastMinute) `div` cycleLength
          usedIteration = final - (lastMinute + nCycles * cycleLength) + prevMinute
          usedWorld = head $ M.keys $ M.filter (== usedIteration) worlds


score :: World -> Int
score world = nTrees * nLumber
    where nTrees = M.size $ M.filter (== Trees) world
          nLumber = M.size $ M.filter (== Lumberyard) world

makeWorld :: [[Cell]] -> World
makeWorld rows = M.unions $ [makeWorldRow r row | (r, row) <- zip [1..] rows]

makeWorldRow :: Int -> [Cell] -> World
makeWorldRow r row = M.fromList [((r, c), cell) | (c, cell) <- zip [1..] row]

neighbours :: Coord -> World -> World
neighbours here world = M.filterWithKey isNeighbour world
    where isNeighbour c _ = c `elem` neighbourCoords here

neighbourCoords :: Coord -> [Coord]
neighbourCoords (r, c) = [(r', c') | r' <- [(r - 1)..(r + 1)]
                                   , c' <- [(c - 1)..(c + 1)]
                                   , ((r' /= r) || (c' /= c))
                                   ]

showWorld world = unlines $ [showWorldRow r world | r <-[minR..maxR]]
    where ((minR, _), _) = M.findMin world
          ((maxR, _), _) = M.findMax world

showWorldRow r world = concat [show (lookupCell (r, c) world) | c <- [minC..maxC]]
    where ((_, minC), _) = M.findMin world
          ((_, maxC), _) = M.findMax world


lookupCell :: Coord -> World -> Cell
lookupCell coord world = M.findWithDefault Open coord world

generation :: World -> World
generation world = M.mapWithKey generationCell world
    where generationCell here _ = propogateCell here world

propogateCell :: Coord -> World -> Cell
propogateCell here world = propogateCell' (world!here)
    where propogateCell' Open = if nTrees >= 3 then Trees else Open
          propogateCell' Trees = if nLumber >= 3 then Lumberyard else Trees
          propogateCell' Lumberyard = if (nLumber >= 1) && (nTrees >= 1) then Lumberyard else Open
          ns = neighbours here world
          nTrees = M.size $ M.filter (== Trees) ns
          nLumber = M.size $ M.filter (== Lumberyard) ns

cacheWorlds :: World -> (Cache, World)
cacheWorlds world = go (M.empty, world, 0) (drop 1 $ iterate generation world)
    where go (cache, prev, minute) [] = (cache, prev)
          go (cache, prev, minute) (w:ws) = 
            if w `M.member` cache
            then (cache', w)
            else go (cache', w, minute + 1) ws
            where cache' = M.insert prev minute cache

-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome (char ' ')) CA.empty CA.empty

-- lexeme  = L.lexeme sc
-- integer = lexeme L.decimal
symb = L.symbol sc

openP = (symb "." *> pure Open)
treesP = (symb "|" *> pure Trees)
lumberP = (symb "#" *> pure Lumberyard)
cellP = openP <|> treesP <|> lumberP

fileP =  rowP `sepEndBy` (char '\n')

rowP = many cellP

successfulParse :: Text -> [[Cell]]
successfulParse input = 
        case parse fileP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right world -> world