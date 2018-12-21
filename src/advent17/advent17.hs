{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List
-- import qualified Data.Set as S

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Tuple (swap)

type SoilSpecLine = ((Text, Integer), (Text, Integer, Integer))
type Coord = (Integer, Integer) -- x, y
data Soil = Sand | Clay | Still | Flowing deriving (Eq, Show, Enum, Bounded, Ord)
type Ground = M.Map Coord Soil

main :: IO ()
main = do 
    text <- TIO.readFile "data/advent17.txt"
    let soilSpec = successfulParse text
    -- print soilSpec
    let ground = makeGround soilSpec
    -- print ground
    -- putStrLn $ showGround ground
    -- putStrLn $ showGround $ handleSource ground (500, 0)
    -- print $ handleSource ground (500, 0)
    let ground' = filled ground
    print $ part1 ground'
    print $ part2 ground'
    -- print $ part1 tests
    -- print $ part2 tests program


part1 ground = M.size $ M.union still flowing
    where (_minX, _maxX, minY, maxY) = bounds ground
          inBoundGround = M.filterWithKey (\(_x, y) _ -> (y >= minY) && (y <= maxY)) ground
          still = M.filter (== Still) inBoundGround
          flowing = M.filter (== Flowing) inBoundGround

part2 ground = M.size $ still
    where (_minX, _maxX, minY, maxY) = bounds ground
          inBoundGround = M.filterWithKey (\(_x, y) _ -> (y >= minY) && (y <= maxY)) ground
          still = M.filter (== Still) inBoundGround    

makeGround :: [SoilSpecLine] -> Ground
makeGround soilSpec = foldl' addSpecLine M.empty soilSpec

addSpecLine :: Ground -> SoilSpecLine -> Ground
addSpecLine ground ((fixed, fixedVal), (ranged, from, to)) =
    foldr (\c -> M.insert c Clay) ground addedCells
    where cells = [(fixedVal, i) | i <- [from..to] ]
          addedCells = if fixed == "x" then cells else map swap cells

showGround :: Ground -> String
showGround ground = unlines $ map (showGroundLine minX maxX ground) [minY..maxY]
    where (minX, maxX, minY, maxY) = bounds ground

showGroundLine :: Integer -> Integer -> Ground -> Integer -> String
showGroundLine minX maxX ground y = [showGroundCell x | x <- [minX..maxX]]
    where showGroundCell x = if (x, y) `M.member` ground
                               then case ground!(x, y) of
                                        Clay -> '#' -- '\x2591'
                                        Flowing -> '|'
                                        Still -> 'o' -- '\x2593'
                                        Sand -> '.'
                                else '.'

bounds :: Ground -> (Integer, Integer, Integer, Integer)
bounds ground = (minX, maxX, minY, maxY)
    where keys = M.keys ground -- $ M.filter (== Clay) ground
          minX = minimum $ map fst keys
          maxX = maximum $ map fst keys
          minY = minimum $ map snd keys
          maxY = maximum $ map snd keys

up (x, y) = (x, y-1)
down (x, y) = (x, y+1)
left (x, y) = (x-1, y)
right (x, y) = (x+1, y)


filled :: Ground -> Ground
filled ground = handleSource ground (500, 0)


handleSource :: Ground -> Coord -> Ground
-- handleSource ground here | trace ("source " ++ show here ++ "\n" ++ showGround ground) False = undefined
handleSource ground here 
    | (snd here) > maxY = ground
    | otherwise = flood ground' here
    where (_minX, _maxX, _minY, maxY) = bounds ground
          under = M.findWithDefault Sand (down here) ground
          ground' = if under == Sand 
                    then handleSource (M.insert here Flowing ground) (down here)
                    else M.insert here Flowing ground

    -- else if (down here) `M.notMember` ground
    --      then handleSource ground' (down here)
    --      else flood ground' here
    -- where (_minX, _maxX, _minY, maxY) = bounds ground
    --       ground' = (M.insert here Water ground)

flood :: Ground -> Coord -> Ground
-- flood ground here | trace ("flood " ++ show here) False = undefined
flood ground here = foldl' handleSource groundB sources
    where (groundL, sourcesL, boundL) = floodLeft ground here
          (groundR, sourcesR, boundR) = floodRight groundL here
          sources = sourcesL ++ sourcesR
          groundB = if boundL && boundR 
                    then stillWater groundR here
                    else groundR


    -- if null sources
    -- then flood groundLR (up here)
    -- else foldl' handleSource groundLR sources
    -- where (groundL, sourcesL) = floodLeft ground here 
    --       (groundLR,  sourcesR) = floodRight groundL here 
    --       sources = sourcesL ++ sourcesR

-- if the ground under is sand, create a new source 
-- otherwise, 
-- if the groudnd to the left is clay, stop
-- if the ground to the left isn't clay, flood left 

floodLeft :: Ground -> Coord -> (Ground, [Coord], Bool)
-- floodLeft ground here | trace ("flood <= " ++ show here) False = undefined
floodLeft ground here = 
    if leftWards == Clay
    then (ground, [], True)
    else case (under, underLeft) of
        (Clay, Sand) -> (ground', [left here], False)
        (Clay, Clay) -> floodLeft ground' (left here)
        (Still, Still) -> floodLeft ground' (left here)
        (Still, Clay) -> floodLeft ground' (left here)
        (Clay, Still) -> floodLeft ground' (left here)
        _             -> (ground, [], False)
    where ground' = (M.insert (left here) Flowing ground)
          under = M.findWithDefault Sand (down here) ground
          leftWards = M.findWithDefault Sand (left here) ground
          underLeft =  M.findWithDefault Sand (left (down here)) ground


floodRight :: Ground -> Coord -> (Ground, [Coord], Bool)
-- floodRight ground here | trace ("flood => " ++ show here) False = undefined
floodRight ground here =
    if rightWards == Clay
    then (ground, [], True)
    else case (under, underRight) of
        (Clay, Sand) -> (ground', [right here], False)
        (Clay, Clay) -> floodRight ground' (right here)
        (Still, Still) -> floodRight ground' (right here)
        (Still, Clay) -> floodRight ground' (right here)
        (Clay, Still) -> floodRight ground' (right here)
        _             -> (ground, [], False)
    where ground' = (M.insert (right here) Flowing ground)
          under = M.findWithDefault Sand (down here) ground
          rightWards = M.findWithDefault Sand (right here) ground
          underRight =  M.findWithDefault Sand (right (down here)) ground
    -- if under == Sand
    -- then (ground', [here])
    -- else if rightWards == Clay
    --      then (ground', [])
    --      else if (under == Water) && (rightWards == Sand) 
    --           then (ground', [])
    --           else floodRight ground' (left here)
    -- where ground' = (M.insert here Water ground)
    --       under = M.findWithDefault Sand (down here) ground
    --       rightWards = M.findWithDefault Sand (right here) ground
-- floodRight ground here = 
    -- case under of
    --     Sand -> (ground', [here])
    --     Water -> (ground', [])
    --     otherwise -> if (right here) `M.notMember` ground
    --                  then floodRight ground' (right here)
    --                  else if ground!(right here) == Water
    --                       then floodRight ground' (right here)
    --                       else (ground', [])
    -- where ground' = (M.insert here Water ground)
    --       under = M.findWithDefault Sand (down here) ground

stillWater :: Ground -> Coord -> Ground
-- stillWater ground here | trace ("stilling " ++ show here) False = undefined
stillWater ground here = stillWaterR groundL here
    where groundL = stillWaterL ground here

stillWaterL :: Ground -> Coord -> Ground
-- stillWaterL ground here | trace ("stilling L" ++ show here) False = undefined
stillWaterL ground here = 
    if ground!(left here) == Clay
    then ground'
    else stillWaterL ground' (left here)
    where ground' =(M.insert here Still ground)

stillWaterR :: Ground -> Coord -> Ground
-- stillWaterR ground here | trace ("stilling R" ++ show here) False = undefined
stillWaterR ground here = 
    if ground!(right here) == Clay
    then ground'
    else stillWaterR ground' (right here)
    where ground' = (M.insert here Still ground)



-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

equalP = symb "="
commaP = symb ","
ellipsisP = ".."
axisP = symb "x" <|> symb "y"

fileP = many rowP

rowP = (,) <$> fixedP <* commaP <*> rangeP

fixedP = (,) <$> axisP <* equalP <*> integer
rangeP = (,,) <$> axisP <* equalP <*> integer <* ellipsisP <*> integer


successfulParse :: Text -> [SoilSpecLine]
successfulParse input = 
        case parse fileP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right soilSpec -> soilSpec