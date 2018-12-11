{-# LANGUAGE OverloadedStrings #-}

import Data.List

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import qualified Data.Set as S

type Coord = (Integer, Integer) -- x, y
type Bounds = (Integer, Integer, Integer, Integer) -- minX, maxX, minY, maxY
data Particle = Particle {_position :: Coord, _velocity :: Coord} deriving (Eq, Show)
type Swarm = [Particle]
type Grid = S.Set Coord

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent10.txt"
        let particles = successfulParse text
        let (final, time) = part0 particles
        putStrLn $ showParticles final
        print time

part0 :: Swarm -> (Swarm, Int)
part0 particles = (snd $ last $ gridPairs, length gridPairs)
    where gridPairs = findEnd particles

runParticles :: Swarm -> [Swarm]
runParticles = iterate updateAll 

findEnd :: Swarm -> [(Swarm, Swarm)]
findEnd particles = takeWhile firstLarger gridPairs
    where grids = runParticles particles
          gridPairs = zip grids (drop 1 grids)
          firstLarger (g1, g2) = (boundsArea g1) > (boundsArea g2)



boundsArea :: Swarm -> Integer
boundsArea particles = (maxX - minX) * (maxY - minY)
    where (minX, maxX, minY, maxY) = findBounds particles

findBounds :: Swarm -> Bounds 
findBounds particles = 
        ( minX -- small x edge
        , maxX -- large x edge
        , minY -- small x edge
        , maxY -- large y edge
        )
    where maxX = maximum $ map (fst . _position) particles
          minX = minimum $ map (fst . _position) particles
          maxY = maximum $ map (snd . _position) particles
          minY = minimum $ map (snd . _position) particles


update :: Particle -> Particle
update particle = particle {_position = (x + vx, y + vy)}
    where (x, y) = _position particle
          (vx, vy) = _velocity particle


updateAll :: Swarm -> Swarm
updateAll = map update

showParticles :: Swarm -> String
showParticles particles = intercalate "\n" rows
    where (minX, maxX, minY, maxY) = findBounds particles
          grid = S.fromList $ map _position particles
          rows = [showRow y minX maxX grid | y <- [minY..maxY] ]

showCell :: Integer -> Integer -> Grid -> Char
showCell x y grid 
    | (x, y) `S.member` grid = '*'
    | otherwise = ' '

showRow :: Integer -> Integer -> Integer -> Grid -> String
showRow y minX maxX grid = [showCell x y grid | x <- [minX..maxX] ]

-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc
signedInteger = L.signed sc integer

posPrefix = symb "position=<"
velPrefix = symb "velocity=<"
suffix = symb ">"
commaP = symb ","

particleFileP = many particleP

particleP = particlify <$> positionP <*> velocityP 
    where particlify x v = Particle x v

positionP = posPrefix *> pairP <* suffix
velocityP = velPrefix *> pairP <* suffix

pairP = (,) <$> signedInteger <* commaP <*> signedInteger

successfulParse :: Text -> Swarm
successfulParse input = 
        case parse particleFileP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right particles -> particles