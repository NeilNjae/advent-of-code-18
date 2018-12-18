import Data.List
-- import Data.Tuple (swap)
import Data.Maybe
import qualified Data.Foldable
import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Set as S
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Sequence as Q
import Data.Sequence ((|>), (<|), Seq (Empty, (:<|), (:|>)))

import Debug.Trace

type Coord = (Int, Int) -- row, column
type HitPoints = Int
data Species = Elf | Goblin deriving (Show, Eq)
data Agent = Agent Species HitPoints deriving (Show, Eq)
type Agents = M.Map Coord Agent
type Layout = S.Set Coord
type OrderedLayout = Q.Seq Coord

type Closed = Layout
data Agendum = Agendum {_current :: Coord, _trail :: OrderedLayout, _cost :: Int} deriving (Show, Eq)
type Agenda = P.MinPQueue (Int, Int) Agendum 

newGoblin = Agent Goblin 200
newElf = Agent Elf 200

isSpecies s (Agent s' _) = s == s'

isElf = isSpecies Elf
isGoblin = isSpecies Goblin

otherSpecies Elf = Goblin
otherSpecies Goblin = Elf

elfs :: Agents -> Agents
elfs agents = M.filter isElf agents

goblins :: Agents -> Agents
goblins agents = M.filter isGoblin agents



test1 = "#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######\n"
test2 = "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########\n"
test3 = "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######\n"

main :: IO ()
main = do 
    text <- readFile "data/advent15.txt"
    let (layout, agents) = parse test3
    print layout
    print agents
    putStrLn $ showWorld layout agents
    print $ pathsToEnemies (1, 1) layout agents
    -- let (h, e) = M.findMin $ elfs agents
    -- print (h, e)
    -- forM_ (M.assocs agents) $ \(ah, a) -> do 
    --     print (ah, a)
    --     print $ pathsToEnemies ah layout agents
    --     print $ bestMove ah layout agents     
    let a1 = doRound layout agents
    putStrLn $ showWorld layout a1
    print a1
    let a2 = doRound layout a1
    putStrLn $ showWorld layout a2
    print a2
    let a3 = doRound layout a2
    putStrLn $ showWorld layout a3
    print a3
    let a4 = doRound layout a3
    putStrLn $ showWorld layout a4
    print a4
    let a5 = doRound layout a4
    putStrLn $ showWorld layout a5
    print a5
    let a6 = doRound layout a5
    putStrLn $ showWorld layout a6
    print a6


showWorld layout agents = unlines rows
    where rows = map (showRow layout agents) [minRow..maxRow]
          minRow = fst $ S.findMin layout'
          maxRow = fst $ S.findMax layout'
          layout' = S.union layout $ S.fromList $ M.keys agents

showRow layout agents row = map (\col -> showCell (row, col) layout agents) [minCol..maxCol]
    where minCol = minimum $ map snd $ S.toList layout'
          maxCol = maximum $ map snd $ S.toList layout'
          layout' = S.union layout $ S.fromList $ M.keys agents

showCell c layout agents = 
    if c `M.member` agents
    then if isElf a then 'E' else 'G'
    else if c `S.member` layout then '.' else '\x2593'
    where a = agents!c


-- Parsing
parse :: String -> (Layout, Agents)
parse text = foldl' parseRow (S.empty, M.empty) $ zip [0..] $ lines text

parseRow :: (Layout, Agents) -> (Int, String) -> (Layout, Agents)
parseRow (layout, agents) (r, row) = foldl' parseCellWithY (layout, agents) $ zip [0..] row
    where parseCellWithY = parseCell r

parseCell :: Int -> (Layout, Agents) -> (Int, Char) -> (Layout, Agents)
parseCell r (layout, agents) (c, cell) = 
    let here = (r, c)
    in case cell of 
            'G' -> (S.insert here layout, M.insert here newGoblin agents)
            'E' -> (S.insert here layout, M.insert here newElf agents)
            '.' -> (S.insert here layout, agents)
            _   -> (layout, agents)

-- Locations

adjacent :: Coord -> Layout -> Layout
adjacent (r, c) layout = S.intersection layout 
                         $ S.fromList [(r+1, c), (r-1, c), (r, c+1), (r, c-1)]

free :: Coord -> Layout -> Agents -> Bool
free here layout agents = (here `S.member` layout) && (here `M.notMember` agents)

adjacentFree :: Coord -> Layout -> Agents -> Layout
adjacentFree here layout agents = S.filter (\c -> free c layout agents) (adjacent here layout)

orderedAdjacentFree :: Coord -> Layout -> Agents -> OrderedLayout
orderedAdjacentFree here layout agents = Q.sort $ S.foldl' (|>) Q.empty cells
    where cells = adjacentFree here layout agents


-- Searching

initAgenda :: Coord -> Coord -> Agenda
initAgenda start goal = P.singleton ((estimateCost start goal), (fst start * 100 + snd start)) Agendum {_current = start, _trail = Q.empty, _cost = 0}

aStar :: Coord -> Layout -> Agents -> Agenda -> Closed -> Maybe Agendum
aStar goal layout agents agenda closed 
    -- | trace ("Peeping " ++ (show $ fst $ P.findMin agenda) ++ ": " ++ (show reached) ++ " <- " ++ (show $ _trail $ currentAgendum) ++ " :: " ++ (show newAgenda)) False = undefined
    | P.null agenda = Nothing
    | otherwise = 
        if reached == goal then Just currentAgendum
        else if reached `S.member` closed 
            then aStar goal layout agents (P.deleteMin agenda) closed
            else aStar goal layout agents newAgenda closed'
        where 
            (_, currentAgendum) = P.findMin agenda
            reached = _current currentAgendum
            closed' = S.insert reached closed
            tieBreakerCost a = foldl' (\t (r, c) -> t + r * 100 + c) 0 ((_current a) <| (_trail a))
            newAgenda = foldl' (\q a -> P.insert ((estimatedCost a), tieBreakerCost a) a q) (P.deleteMin agenda) $ candidates layout agents currentAgendum closed'
            estimatedCost agendum = estimateCost reached goal + _cost agendum

candidates :: Layout -> Agents -> Agendum -> Closed -> Q.Seq Agendum
candidates layout agents agendum closed = newCandidates
    where
        candidate = _current agendum
        previous = _trail agendum
        succs = orderedAdjacentFree candidate layout agents
        nonloops = Q.filter (\s -> s `S.notMember` closed) succs
        newCandidates = fmap (\n -> makeAgendum n) nonloops
        makeAgendum new = Agendum { _current = new, 
                                    _trail = candidate <| previous, 
                                    _cost = _cost agendum + 1}

estimateCost :: Coord -> Coord -> Int
estimateCost (r, c) (gr, gc) = abs (r - gr) + abs(c - gc)

-- Move selection

shortestDistanceTo :: Coord -> Coord -> Layout -> Agents -> Maybe (Int, OrderedLayout)
shortestDistanceTo here there layout agents = 
    if searchResult == Nothing 
    then Nothing
    else Just (_cost $ fromJust searchResult, (_current $ fromJust searchResult) <| (_trail $ fromJust searchResult))
    where searchResult = aStar there layout agents (initAgenda here there) S.empty

enemyLocations :: Coord -> Agents -> Layout
enemyLocations here agents = S.fromList $ M.keys $ M.filter (isSpecies enemySpecies) agents
    where Agent thisSpecies _ = agents!here
          enemySpecies = otherSpecies thisSpecies

agentTargets :: Coord -> Layout -> Agents -> Layout
agentTargets here layout agents = S.foldl S.union S.empty enemyAdjacents
    where enemies = enemyLocations here agents
          enemyAdjacents = S.map (\l -> adjacentFree l layout agents) enemies

pathsToEnemies :: Coord -> Layout -> Agents -> [(Int, OrderedLayout)]
pathsToEnemies here layout agents = catMaybes $ map sdt $ S.toList targets
    where sdt there = shortestDistanceTo here there layout agents
          targets = agentTargets here layout agents

closestEnemies :: Coord -> Layout -> Agents -> [OrderedLayout]
closestEnemies here layout agents = possibles
    where paths = pathsToEnemies here layout agents
          closest = minimum $ map fst paths
          possibles = map snd $ filter (\p -> fst p == closest) paths

bestMove :: Coord -> Layout -> Agents -> Coord
bestMove here layout agents = 
    if null paths
    then here
    else head $ sort $ map pathStep paths
    where paths = closestEnemies here layout agents
          pathStep p = if Q.length p > 1 then Q.index p (Q.length p - 2) else Q.index p 1

makeBestMove :: Coord -> Layout -> Agents -> Agents
makeBestMove here layout agents = M.insert there agent $ M.delete here agents
    where agent = agents!here
          there = bestMove here layout agents

-- Attacking

bestTarget :: Coord -> Layout -> Agents -> Coord
bestTarget here layout agents = keyOfMinHP $ M.filterWithKey (\c _ -> c `S.member` enemies) agents
    where enemies = touchingEnemies here layout agents

attack :: Coord -> Agents -> Agents
attack target agents = M.insert target (Agent species (hp - 3)) agents
    where Agent species hp = agents!target

keyOfMinHP :: Agents -> Coord -- Ord b => M.Map a b -> a
keyOfMinHP m = fst $ M.foldrWithKey mergeKV (M.findMin m) m
    where mergeKV k (Agent s v) (bestK, (Agent sb bestV)) = 
            if v < bestV then (k, (Agent s v)) else (bestK, (Agent sb bestV))

makeAttack :: Coord -> Layout -> Agents -> Agents
makeAttack here layout agents = attack target agents
    where target = bestTarget here layout agents

-- Game loop          

doRound :: Layout -> Agents -> Agents
doRound layout agents = agents'
    -- where agents' = foldl' (\a h -> makeBestMove h layout a) agents $ M.keys agents
    where agents' = foldl' (\a h -> agentAction h layout a) agents $ M.keys agents

touchingEnemies :: Coord -> Layout -> Agents -> Layout
touchingEnemies here layout agents = S.intersection neighbourhood enemies
    where neighbourhood = adjacent here layout 
          enemies = enemyLocations here agents

agentAction :: Coord -> Layout -> Agents -> Agents
agentAction here layout agents = 
    if S.null targets
    then makeBestMove here layout agents
    else makeAttack here layout agents
    where targets = touchingEnemies here layout agents
