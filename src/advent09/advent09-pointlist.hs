{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternSynonyms #-}

import Data.List

import Data.Maybe (fromJust)

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

-- import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M

import Data.List.PointedList (PointedList)
import qualified Data.List.PointedList.Circular as PL

type Circle = PointedList Integer
type Score = M.Map Integer Integer -- player -> score
data Game = Game Circle Score deriving (Show, Eq)

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent09.txt"
        let (numberOfPlayers, numberOfMarbles) = successfulParse text
        print $ part1 numberOfPlayers numberOfMarbles
        print $ part1 numberOfPlayers (numberOfMarbles * 100)

part1 :: Integer -> Integer -> Integer
part1 players marbles = highScore $ playGame players marbles

playGame :: Integer -> Integer -> Game
-- playGame players marbles = scanl makeMove createGame $ zip (cycle [1..players]) [1..marbles]
playGame players marbles = foldl' makeMove createGame $ zip (cycle [1..players]) [1..marbles]

highScore :: Game -> Integer
highScore (Game _ score) = maximum $ M.elems score

createGame :: Game
createGame = Game (createCircle 0) M.empty

createCircle :: Integer -> Circle
createCircle current = PL.singleton current


makeMove :: Game -> (Integer, Integer) -> Game
makeMove (Game circle score) (player, marble) =
    if marble `mod` 23 == 0
    then let circle' = (iterate PL.previous circle) !! 7
             score' = updateScore score player (marble + (PL._focus circle'))
             circle'' = fromJust $ PL.deleteRight circle'
         in Game circle'' score'
    else let circle' = PL.insertRight marble (PL.next circle)
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