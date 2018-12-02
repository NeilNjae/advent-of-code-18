import Data.List

main :: IO ()
main = do 
        text <- readFile "data/advent02.txt"
        let ids = lines text
        print $ part1 ids
        putStrLn $ part2 ids

part1 ids = (fst counts23) * (snd counts23)
    where allLetterCounts = map letterCounts ids
          counts23 = foldl' addCounts (0, 0) allLetterCounts

letterCounts :: String -> [Int]
letterCounts = map length . group . sort

addCounts :: (Int, Int) -> [Int] -> (Int, Int)
addCounts (twos, threes) counts = (twos', threes')
    where twos'   = if 2 `elem` counts then twos + 1   else twos
          threes' = if 3 `elem` counts then threes + 1 else threes

part2 ids = uncurry intersect closeIds
    where closeIds = head $ filter (\ab -> uncurry differenceCount ab == 1) 
                        [(a, b) | a:rest <- tails ids, b <- rest]

differenceCount :: String -> String -> Int
differenceCount this that = length $ filter (\(a, b) -> a /= b) $ zip this that
