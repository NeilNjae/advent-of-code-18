{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


-- import Debug.Trace

-- import Prelude hiding ((++))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import qualified Data.Set as S

import Linear (V2(..))

-- import Control.Monad.State.Lazy
import Control.Monad.State.Strict
import Control.Monad.Extra (concatMapM)
-- import Control.Monad.Trans.List

type Coord = V2 Integer -- x, y, with north and east incresing values (origin a bottom left)
data Door = Door Coord Coord deriving (Show, Eq, Ord)
type Doors = S.Set Door

data MazeSection = Path [Coord] | Junction [Maze] deriving (Show, Eq)
type Maze = [MazeSection]


type Mapper = State Doors [Coord]


makeDoor :: Coord -> Coord -> Door
makeDoor !a !b 
    | a < b = Door a b
    | otherwise = Door b a

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent20.txt"
        let maze = successfulParse text
        print $ T.length text
        -- print maze
        part1 maze


emptyMap = S.empty

part1 maze = 
    do 
        let start = V2 0 0
        let doors = execState (mapMaze [start] maze) emptyMap
        print $ length doors


mapMaze :: [Coord] -> Maze -> Mapper
mapMaze !starts !sections =
    foldM (\heres section -> mapMazeSection heres section) starts sections

mapMazeSection :: [Coord] -> MazeSection -> Mapper
mapMazeSection !starts (Junction mazes) = 
    concatMapM (\maze -> mapMaze starts maze) mazes
mapMazeSection !starts (Path steps) = 
    mapM mapPath starts
    where mapPath start = foldM (\here step -> includeDoor here step) start steps

includeDoor :: Coord -> Coord -> State Doors Coord
includeDoor !here !step = 
    do let there = (here + step)
       let door = there `seq` makeDoor here there
       modify' (door `seq` S.insert door)
       return there


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

-- lexeme  = L.lexeme sc
symb = L.symbol sc
branchSepP = symb "|"
openBranchP = symb "("
closeBranchP = symb ")"
startP = symb "^"
endP = symb "$"

doorP :: Parser Coord
doorP = nP <|> sP <|> eP <|> wP
nP = (symb "N" *> pure (V2  0  1))
sP = (symb "S" *> pure (V2  0 -1))
eP = (symb "E" *> pure (V2  1  0))
wP = (symb "W" *> pure (V2 -1  0))

pathP = Path <$> some doorP

junctionP = Junction <$> (openBranchP `between` closeBranchP) manyMazesP

manyMazesP = mazeP `sepBy` branchSepP

mazeSectionP = pathP <|> junctionP

mazeP = many mazeSectionP

wholeMazeP = (startP `between` endP) mazeP


successfulParse :: Text -> Maze
successfulParse input = 
        case parse wholeMazeP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right maze   -> maze
