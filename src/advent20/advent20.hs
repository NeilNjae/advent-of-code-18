{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}


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

import Control.Monad.State.Lazy

type Coord = V2 Integer -- x, y, with north and east incresing values (origin a bottom left)
data Door = Door Coord Coord deriving (Show, Eq, Ord)
type Doors = S.Set Door

makeDoor :: Coord -> Coord -> Door
makeDoor a b 
    | a < b = Door a b
    | otherwise = Door b a




main :: IO ()
main = do 
        text <- TIO.readFile "data/advent20.txt"
        print $ T.length text
        -- let (ip, instrs) = successfulParse text
        -- print (ip, instrs)
        -- -- print $ part1 ip instrs
        -- print $ sum [i | i <- [1..1032], 1032 `mod` i == 0]
        -- -- print $ part2 ip instrs
        -- print $ sum [i | i <- [1..10551432], 10551432 `mod` i == 0]



-- type Parser = Parsec Void Text
type Parser = ParsecT Void Text (StateT [Coord] [])


sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
-- integer = lexeme L.decimal
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

-- instructionFileP = (startP `between` endP) branchesP

-- branchesP :: MyParser Doors
-- branchesP = fmap S.unions . many $ choiceP <|> pathP

-- choiceP :: MyParser Doors
-- choiceP = (openBranchP `between` closeBranchP)  $ do
--     here <- get
--     return fmap S.unions (`sepBy` branchSepP) $ do
--         put here
--         return branchesP



-- pathP :: MyParser Doors
-- pathP = S.fromList <$> many stepP
pathP = many stepP

-- stepP :: MyParser Door
stepP = do
    heres <- get
    delta <- doorP
    let theres = map (+delta) heres 
    put theres
    return (map (\h -> makeDoor h (h + delta)) heres)

-- choiceP :: MyParser [Door]
-- choiceP = (openBranchP `between` closeBranchP) $ do
--     heres <- get
--     do 
--         here <- heres
--         put [here]
--         branch <- (pathP `sepBy` branchSepP)
--         return branch

    -- fmap concat $ (`sepBy` branchSepP) $ do
    --     here <- heres 
    --     pathP


{-    heres <- get
    choices <- (`sepBy` branchSepP) $ do
        put heres
        pathP
    return $ concat choices -- (S.unions choices)-}

-- choiceOrPathP = many (choiceP <|> pathP)


-- successfulParse :: Text -> (Integer, [Instruction])
-- successfulParse input = 
--         case parse instructionsP "input" input of
--                 Left  _error -> (0, []) -- TIO.putStr $ T.pack $ parseErrorPretty err
--                 Right instructions  -> instructions