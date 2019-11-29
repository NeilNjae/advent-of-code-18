{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Debug.Trace

-- import Prelude hiding ((++))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List hiding (group)
-- import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Linear (V4(..), (^-^))


type Coord = V4 Integer
type Constellations = M.Map Coord Coord


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent25.txt"
        let stars = successfulParse text
        print $ part1 stars


part1 :: [Coord] -> Int
part1 stars = countConstellations constellations
    where initialConstellations = foldl' (\c s -> M.insert s s c) M.empty stars
          pairs = adjacencies stars
          constellations = findConstellations initialConstellations pairs


manhattan :: Coord -> Integer
manhattan (V4 x y z t) = (abs x) + (abs y) + (abs z) + (abs t)

distance :: Coord -> Coord -> Integer
distance a b = manhattan (a ^-^ b)

close :: Coord -> Coord -> Bool
close a b = (distance a b) <= 3

adjacencies :: [Coord] -> [(Coord, Coord)]
adjacencies stars = filter (\(a, b) -> a /= b && (close a b))
                    [ (a, b) | a <- stars, b <- stars ]


findConstellations :: Constellations -> [(Coord, Coord)] -> Constellations
findConstellations constellations0 pairs = foldl' mergeConstellation constellations0 pairs

mergeConstellation :: Constellations -> (Coord, Coord) -> Constellations
mergeConstellation constellations (a, b) = 
    if ae == be
    then constellations
    else M.insert ae be constellations
    where ae = exemplar constellations a
          be = exemplar constellations b

exemplar :: Constellations -> Coord -> Coord
exemplar constellations star = 
    if constellations!star == star
    then star
    else exemplar constellations (constellations!star)


countConstellations :: Constellations -> Int
countConstellations constellations = M.size $ M.filterWithKey (==) constellations


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
signedInteger = L.signed sc integer
symb = L.symbol sc
comma = symb ","

coordP = V4 <$> signedInteger <* comma 
            <*> signedInteger <* comma 
            <*> signedInteger <* comma 
            <*> signedInteger

starsP = many coordP

successfulParse :: Text -> [Coord]
-- successfulParse _ = []
successfulParse input = 
        case parse starsP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right stars -> stars
