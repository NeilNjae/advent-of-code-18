{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternSynonyms #-}

import Data.List

import Data.Foldable (toList)

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

-- import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M

import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>), ViewL((:<)), ViewR((:>)) )

-- zipper of left, current, right
data Circle = Circle (Q.Seq Integer) Integer (Q.Seq Integer) deriving (Eq)
type Score = M.Map Integer Integer -- player -> score
data Game = Game Circle Score deriving (Show, Eq)

instance Show Circle where
    show (Circle left current right) = (showSide left) ++ " (" ++ (show current) ++ ") " ++ (showSide right)
        where showSide s = intercalate " " $ map show $ toList s

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent09.txt"
        let (numberOfPlayers, numberOfMarbles) = successfulParse text
        -- let numberOfPlayers = 10
        -- let numberOfMarbles = 1618
        -- print $ take 5 $ scanl (\c n -> insertAfter n $ stepClockwise c) (createCircle 0) [1..]
        -- print $ playGame numberOfPlayers numberOfMarbles
        -- print (let p = 10 ; m = 1618 in part1 p m)
        -- print (let p = 13 ; m = 7999 in part1 p m)
        -- print (let p = 17 ; m = 1104 in part1 p m)
        -- print (let p = 21 ; m = 6111 in part1 p m)
        -- print (let p = 30 ; m = 5807 in part1 p m)
        print $ part1 numberOfPlayers numberOfMarbles
        print $ part1 numberOfPlayers (numberOfMarbles * 100)


        -- putStrLn $ part1 schedule
        -- print $ part2 schedule

part1 players marbles = highScore $ playGame players marbles

playGame :: Integer -> Integer -> Game
-- playGame players marbles = scanl makeMove createGame $ zip (cycle [1..players]) [1..marbles]
playGame players marbles = foldl' makeMove createGame $ zip (cycle [1..players]) [1..marbles]

highScore :: Game -> Integer
highScore (Game _ score) = maximum $ M.elems score

createGame :: Game
createGame = Game (createCircle 0) M.empty

createCircle :: Integer -> Circle
createCircle current = Circle Q.empty current Q.empty

currentMarble :: Circle -> Integer
currentMarble (Circle _ m _) = m

stepClockwise :: Circle -> Circle
stepClockwise (Circle left current right)
    | (Q.null left) && (Q.null right) = Circle left current right
    | (Q.null right) = stepClockwise (Circle Q.empty current left)
    | otherwise = Circle (left |> current) r rs
    where (r :< rs) = Q.viewl right

stepAntiClockwise :: Circle -> Circle
stepAntiClockwise (Circle left current right)
    | (Q.null left) && (Q.null right) = Circle left current right
    | (Q.null left) = stepAntiClockwise (Circle right current Q.empty)
    | otherwise = Circle ls l (current <| right)
    where (ls :> l) = Q.viewr left

insertAfter :: Integer -> Circle -> Circle
insertAfter new (Circle left current right) = Circle (left |> current) new right

removeCurrent :: Circle -> Circle
removeCurrent (Circle left _ right) 
    | Q.null right = Circle ls l Q.empty
    | otherwise = Circle left r rs
    where (l :< ls) = Q.viewl left
          (r :< rs) = Q.viewl right

makeMove :: Game -> (Integer, Integer) -> Game
makeMove (Game circle score) (player, marble) =
    if marble `mod` 23 == 0
    then let circle' = (iterate stepAntiClockwise circle) !! 7
             score' = updateScore score player (marble + (currentMarble circle'))
             circle'' = removeCurrent circle'
         in Game circle'' score'
    else let circle' = insertAfter marble (stepClockwise circle)
         in Game circle' score

updateScore :: Score -> Integer -> Integer -> Score
updateScore score player change = M.insert player (current + change) score 
    where current = M.findWithDefault 0 player score


-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

infixP = symb "players; last marble is worth"
suffixP = symb "points"


-- linkP = pairify <$> prefixP <*> upperChar <* infixP <*> upperChar <* suffixP 
--     where pairify _ a b = (a, b)
gameFileP = (,) <$> integer <* infixP <*> integer <* suffixP 

successfulParse :: Text -> (Integer, Integer)
successfulParse input = 
        case parse gameFileP "input" input of
                Left  _error -> (0, 0) -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right game -> game