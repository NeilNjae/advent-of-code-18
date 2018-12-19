import Data.List
import Data.Tuple (swap)
import Data.Maybe
-- import Data.Foldable (forM_)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Set as S

type Coord = (Int, Int) -- row, column
type HitPoints = Int
data Species = Elf | Goblin deriving (Show, Eq)
data Agent = Agent Species HitPoints deriving (Show, Eq)
type Agents = M.Map Coord Agent
type Layout = S.Set Coord
type Distances = M.Map Coord Int

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

-- test1 = "#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######\n"
-- test2 = "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########\n"
-- test3 = "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######\n"
-- test4 = "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######\n"
-- test5 = "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######\n"
-- test6 = "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######\n"
-- test7 = "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######\n"
-- test8 = "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########\n"

main :: IO ()
main = do 
    text <- readFile "data/advent15.txt"
    let (layout, agents) = parse text
    print $ part1 layout agents
    print $ part2 layout agents
 --   print layout
 --   print agents
    -- putStrLn $ showWorld layout agents
    -- let game = runGame layout agents 15
    -- print $ length game
    -- putStrLn $ showWorld layout $ snd $ last game
    -- print $ last game
    -- print $ length game - 1
    -- print $ scoreGame game
    -- print $ wonWithoutLoss game
    -- let game2 = runGame layout agents 14
    -- print $ length game2
    -- putStrLn $ showWorld layout $ snd $ last game2
    -- print $ last game2
    -- print $ length game2 - 1
    -- print $ scoreGame game2
    -- print $ wonWithoutLoss game2
    -- print $ bestMove (1, 1) layout agents
    -- let (h, e) = M.findMin $ elfs agents
    -- print (h, e)
    -- forM_ (M.assocs agents) $ \(ah, a) -> do 
    --     print (ah, a)
        -- print $ pathsToEnemies ah layout agents
        -- print $ bestMove ah layout agents   
    -- let a1 = doRound layout agents
    -- let a0 = doRound layout agents
    -- putStrLn $ "1\n" ++ showWorld layout a0
    -- print a0
    -- let n = 35
    -- let a1 = doNRounds n layout agents
    -- putStrLn $ show n ++ "\n" ++ showWorld layout a1
    -- print a1
    -- let a2 = doRound layout a1
    -- putStrLn $ show (n+1) ++ "\n" ++ showWorld layout a2
    -- print a2
    -- let a3 = doRound layout a2
    -- putStrLn $ show (n+2) ++ "\n" ++ showWorld layout a3
    -- print a3
    -- let a4 = doRound layout a3
    -- putStrLn $ show (n+3) ++ "\n" ++ showWorld layout a4
    -- print a4
    -- let a5 = doRound layout a4
    -- putStrLn $ show (n+4) ++ "\n" ++ showWorld layout a5
    -- print a5

    -- let a6 = doRound layout a5
    -- putStrLn $ "27\n" ++ showWorld layout a6
    -- print a6
    -- let a7 = doRound layout a6
    -- putStrLn $ "28\n" ++ showWorld layout a7
    -- print a7
    -- let a8 = doNRounds 19 layout a7
    -- putStrLn $ "47\n" ++ showWorld layout a8
    -- print a8
    -- let a9 = doRound layout a8
    -- putStrLn $ "30\n" ++ showWorld layout a9
    -- print a9
    -- let aa = doRound layout a9
    -- putStrLn $ "31\n" ++ showWorld layout aa
    -- print aa
    -- let ab = doRound layout aa
    -- putStrLn $ "32\n" ++ showWorld layout ab
    -- print ab
    -- let ac = doRound layout ab
    -- putStrLn $ "33\n" ++ showWorld layout ac
    -- print ac
    -- let ad = doRound layout ac
    -- putStrLn $ "34\n" ++ showWorld layout ad
    -- print ad
    -- let ae = doRound layout ad
    -- putStrLn $ "35\n" ++ showWorld layout ae
    -- print ae


part1 layout agents = scoreGame $ runGame layout agents 3

part2 layout agents = runPart2 layout agents 4

runPart2 layout agents elfPower = 
    if wonWithoutLoss game
    then (scoreGame game, elfPower)
    else runPart2 layout agents (elfPower + 1)
    where game = runGame layout agents elfPower


-- showWorld layout agents = unlines rows
--     where rows = map (showRow layout agents) [minRow..maxRow]
--           minRow = fst $ S.findMin layout'
--           maxRow = fst $ S.findMax layout'
--           layout' = S.union layout $ S.fromList $ M.keys agents

-- showRow layout agents row = map (\col -> showCell (row, col) layout agents) [minCol..maxCol]
--     where minCol = minimum $ map snd $ S.toList layout'
--           maxCol = maximum $ map snd $ S.toList layout'
--           layout' = S.union layout $ S.fromList $ M.keys agents

-- showCell c layout agents = 
--     if c `M.member` agents
--     then if isElf a then 'E' else 'G'
--     else if c `S.member` layout then '.' else '\x2593'
--     where a = agents!c


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

-- Move selection

distancesFrom :: Coord -> Layout -> Agents -> Distances
distancesFrom here layout agents = distanceFlood layout agents [here] (M.singleton here 0)

distanceFlood :: Layout -> Agents -> [Coord] -> Distances -> Distances
distanceFlood layout agents boundary distances
    | null boundary = distances
    | otherwise = distanceFlood layout agents newBoundary newDistances
    where current = head boundary
          currentCost = distances!current
          neighbours = filter (\c -> c `M.notMember` distances) $ S.toList $ adjacentFree current layout agents
          newDistances = foldl' (\m l -> M.insert l (currentCost + 1) m) distances neighbours
          newBoundary = nub $ (tail boundary) ++ neighbours


shortestDistanceStepTo :: Coord -> Coord -> Layout -> Agents -> Maybe (Int, Coord)
shortestDistanceStepTo here there layout agents = 
    if M.null distanceSteps
    then Nothing
    else Just $ head $ sort $ map swap $ M.toList distanceSteps
    where distances = distancesFrom there layout agents
          steps = adjacentFree here layout agents
          distanceSteps = M.filterWithKey (\k _ -> k `S.member` steps) distances -- S.map (\c -> (distances!c, c)) steps

stepsTowardsEnemies :: Coord -> Layout -> Agents -> [(Int, Coord)]
stepsTowardsEnemies here layout agents = 
      catMaybes 
    $ map (\e -> shortestDistanceStepTo here e layout agents) 
    $ S.toList $ enemyLocations here agents

enemyLocations :: Coord -> Agents -> Layout
enemyLocations here agents = S.fromList $ M.keys $ M.filter (isSpecies enemySpecies) agents
    where Agent thisSpecies _ = agents!here
          enemySpecies = otherSpecies thisSpecies

bestMove :: Coord -> Layout -> Agents -> Coord
bestMove here layout agents = 
    if null steps
    then here
    else snd $ head $ sort $ steps
    where steps = stepsTowardsEnemies here layout agents

makeMove :: Coord -> Coord -> Agents -> Agents
makeMove here there agents = M.insert there agent $ M.delete here agents
    where agent = agents!here

-- Attacking

bestTarget :: Coord -> Layout -> Agents -> Coord
bestTarget here layout agents = keyOfMinHP $ M.filterWithKey (\c _ -> c `S.member` enemies) agents
    where enemies = touchingEnemies here layout agents

attack :: Coord -> Agents -> Int -> Agents
attack target agents elfPower = if hp > power
                       then M.insert target (Agent species (hp - power)) agents
                       else M.delete target agents 
    where Agent species hp = agents!target
          power = if species == Goblin then elfPower else 3

keyOfMinHP :: Agents -> Coord -- Ord b => M.Map a b -> a
keyOfMinHP m = fst $ M.foldrWithKey mergeKV (M.findMin m) m
    where mergeKV k (Agent s v) (bestK, (Agent sb bestV)) = 
            if v < bestV then (k, (Agent s v)) else (bestK, (Agent sb bestV))

makeAttack :: Coord -> Layout -> Agents -> Int -> Agents
makeAttack here layout agents elfPower = attack target agents elfPower
    where target = bestTarget here layout agents

-- Game loop          

-- doNRounds :: Int -> Layout -> Agents -> Int -> Agents
-- doNRounds n layout agents elfPower
--     | n == 0 = agents
--     | otherwise = doNRounds (n-1) layout (snd $ doRound layout agents elfPower) elfPower


doRound :: Layout -> Agents -> Int -> (Bool, Agents)
doRound layout agents elfPower = agents'
    where agents' = foldl' (\(_, a) h -> agentAction h layout a elfPower) (True, agents) $ M.keys agents

touchingEnemies :: Coord -> Layout -> Agents -> Layout
touchingEnemies here layout agents = S.intersection neighbourhood enemies
    where neighbourhood = adjacent here layout 
          enemies = enemyLocations here agents

agentAction :: Coord -> Layout -> Agents -> Int -> (Bool, Agents)
agentAction here layout agents elfPower
    | (M.null (elfs agents)) || (M.null (goblins agents)) = (False, agents)
    | here `M.notMember` agents = (True, agents)
    | S.null $ enemyLocations here agents = (True, agents)
    | otherwise = (True, agents'')
    where targets = touchingEnemies here layout agents
          here' = if S.null targets
                  then bestMove here layout agents
                  else here
          agents' = makeMove here here' agents
          targets' = touchingEnemies here' layout agents'
          agents'' = if S.null targets'
                     then agents'
                     else makeAttack here' layout agents' elfPower

runGame :: Layout -> Agents -> Int -> [(Bool, Agents)]
runGame layout agents elfPower = states ++ [doRound layout (snd $ last states) elfPower]
    where states = takeWhile (\(f, _) -> f == True) $ iterate (\(_, a) -> doRound layout a elfPower) (True, agents)

scoreGame :: [(Bool, Agents)] -> Int
scoreGame states = (length states - 2) * hps
    where hps = sum $ map (\(Agent _ hp) -> hp) $ M.elems $ snd $ last states

wonWithoutLoss :: [(Bool, Agents)] -> Bool
wonWithoutLoss game = startingElfs == finishingElfs
    where startingElfs = M.size $ elfs $ snd $ head game
          finishingElfs = M.size $ elfs $ snd $ last game
