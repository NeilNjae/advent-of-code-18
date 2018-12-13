{-# LANGUAGE OverloadedStrings #-}


import Data.List
import qualified Data.Set as S

data Cell = Empty | Horizontal | Vertical | Clockwise | Anticlockwise | Junction deriving (Show, Eq)
data Direction = Up | Right | Down | Left deriving (Show, Eq, Ord, Enum, Bounded)

main :: IO ()
main = do 
    text <- TIO.readFile "data/advent12.txt"
    let (initial, rules) = successfulParse text
    let row = makeWorld 0 initial
    print $ part1 rules row
    print $ part2 rules row



-- Move in the current direction
takeStep :: Direction -> Cell -> Int -> Int -> (Direction, Int, Int)
takeStep Up x y = (x, y-1)
takeStep Down x y = (x, y+1)
takeStep Left x y = ( x-1, y)
takeStep Right x y = (x+1, y)


-- | a `succ` that wraps 
turnCW :: (Bounded a, Enum a, Eq a) => a -> a 
turnCW dir | dir == maxBound = minBound
           | otherwise = succ dir

-- | a `pred` that wraps
turnACW :: (Bounded a, Enum a, Eq a) => a -> a
turnACW dir | dir == minBound = maxBound
            | otherwise = pred dir



-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

symb = L.symbol sc
potP = (char '.' *> pure False) <|> (char '#' *> pure True)

initialPrefix = symb "initial state:"
ruleSepP = symb "=>"

fileP = (,) <$> initialP <*> many ruleP
initialP = initialPrefix *> many potP <* sc
ruleP = Rule <$> ruleLHS <* ruleSepP <*> ruleRHS
ruleLHS = count 5 potP <* sc
ruleRHS = potP <* sc

successfulParse :: Text -> ([Bool], [Rule])
successfulParse input = 
        case parse fileP "input" input of
                Left  _error -> ([], []) -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right world -> world
