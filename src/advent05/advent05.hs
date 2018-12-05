import Data.List
import Data.Char (toLower)

main :: IO ()
main = do 
        text <- readFile "data/advent05.txt"
        -- let ids = lines text
        print $ part1 text
        print $ part2 text
        -- putStrLn $ part2 ids


part1 = reactedLength

part2 polymer = minimum finalLengths
    where finalLengths = map (\u -> reactedLength $ removeUnit polymer u) units
          units = unitsPresent polymer


react :: String -> Char -> Char -> String -> (String, String)
react prefix a b suffix = 
    if willReact a b
    then (prefix, suffix)
    else ((b:a:prefix), suffix)

willReact :: Char -> Char -> Bool
willReact a b = (a /= b) && (toLower a == toLower b)


reactHere :: (String, String) -> Maybe ((String, String), (String, String))
reactHere (prefix, suffix) = 
    if canContinue prefix suffix
    then Just ((prefix'', suffix''), (prefix'', suffix''))
    else Nothing
    where (prefix', a, b, suffix') = reactionSite prefix suffix
          (prefix'', suffix'') = react prefix' a b suffix'


canContinue (_:_) (_:_) = True
canContinue [] (_:_:_) = True
canContinue _ _ = False

reactionSite (a:prefix) (b:suffix) = (prefix, a, b, suffix)
reactionSite [] (a:b:suffix) = ([], a, b, suffix)


reactedLength polymer = length prefix + length suffix
    where (prefix, suffix) = last $ unfoldr reactHere ("", polymer)


unitsPresent = nub . sort . map toLower 

removeUnit polymer unit = filter (\c -> toLower c /= unit ) polymer 
