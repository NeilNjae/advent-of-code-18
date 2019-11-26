{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

-- Box division approach taken from fizbin: 
-- https://www.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecfmpy0/

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

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

import Data.List

import Linear (V3(..), (^-^))

import qualified Data.PQueue.Max as P


type Coord = V3 Integer -- x, y, z 
type BotSwarm = M.Map Coord Integer
-- type VertexCounts = M.Map Coord Integer
type Box = (Coord, Coord)
data LabelledBox = LabelledBox { _box :: Box
                               , _intersectCount :: Int
                               } deriving (Eq, Show)
-- instance Ord LabelledBox where
--     lb1 `compare` lb2 = if (_intersectCount lb1) /= (_intersectCount lb2)
--                         then (_intersectCount lb1) `compare` (_intersectCount lb2)
--                         else if (boxSize lb1) /= (boxSize lb2)
--                              then (boxSize lb1) `compare` (boxSize lb2)
--                              else (distanceFromOrigin lb1) `compare` (distanceFromOrigin lb2)
--     where boxSize lb = manhattan (fst $ _box lb) (snd $ _box lb)
--           distanceFromOrigin lb = min (distance $ fst $ _box lb) (distance $ snd $ _box lb)
instance Ord LabelledBox where
    lb1 `compare` lb2 = if (_intersectCount lb1) /= (_intersectCount lb2)
                        then (_intersectCount lb1) `compare` (_intersectCount lb2)
                        else if (boxSize lb1) /= (boxSize lb2)
                             then (boxSize lb1) `compare` (boxSize lb2)
                             else (distanceFromOrigin $ _box lb1) `compare` (distanceFromOrigin $ _box lb2)
    
boxSize lb = manhattan (fst $ _box lb) (snd $ _box lb)
distanceFromOrigin lb = boxDistanceFromPoint lb origin

type BoxQueue = P.MaxQueue LabelledBox


origin :: Coord
origin = V3 0 0 0


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent23.txt"
        let bots = successfulParse text
        let swarm = enSwarm bots
        print $ part1 swarm
        print $ part2 swarm
        -- print (ip, instrs)
        -- print $ zip [0..] instrs
        -- print $ part1 ip instrs
        -- print $ part2 ip instrs


part1 :: BotSwarm -> Int
part1 swarm = M.size inRangeBots
    where centre = strongest swarm
          range = swarm!centre
          botInRange loc _ = (manhattan loc centre) <= range
          inRangeBots = M.filterWithKey botInRange swarm


-- part2 swarm = ((distance $ snd best), best)
--     where vcs = vertexCounts vs swarm
--           vs = verticesOfSwarm swarm
--           best = targetVertex vcs




manhattan :: Coord -> Coord -> Integer
-- manhattan $ (V3 0 0 0) ^-^ (V3 1 3 1)
manhattan (V3 x y z) (V3 x' y' z') = (abs (x - x')) + (abs (y - y')) + (abs (z - z'))

distance :: Coord -> Integer
distance = manhattan origin

enSwarm :: [(Coord, Integer)] -> BotSwarm
enSwarm = foldl' (\s (c, r) -> M.insert c r s) M.empty


strongest :: BotSwarm -> Coord
strongest swarm = fst $ M.foldlWithKey' findStrongest pair0 swarm
    where findStrongest (currentCoord, currentMax) coord range = 
                if range > currentMax
                then (coord, range)
                else (currentCoord, currentMax)
          pair0 = M.findMin swarm


boxIntersectionCount :: Box -> BotSwarm -> Int
boxIntersectionCount box swarm = M.size $ M.filterWithKey (\b _ -> intersects box b swarm) swarm

intersects :: Box -> Coord -> BotSwarm -> Bool
intersects box bot swarm =
    d <= range
    where d = boxDistanceFromPoint box bot
          range = swarm!bot


boxDistanceFromPoint :: Box -> Coord -> Integer
boxDistanceFromPoint ((V3 l f t), (V3 r b u)) (V3 x y z) = d

    -- # returns whether box intersects bot
    -- d = 0
    -- for i in (0, 1, 2):
    --     boxlow, boxhigh = box[0][i], box[1][i] - 1
    --     d += abs(bot[i] - boxlow) + abs(bot[i] - boxhigh)
    --     d -= boxhigh - boxlow
    -- d //= 2
    -- return d <= bot[3]

    -- dmin = 0;
    --     for( i = 0; i < 3; i++ ) {
    --         if( C[i] < Bmin[i] ) dmin += SQR( C[i] - Bmin[i] ); else
    --         if( C[i] > Bmax[i] ) dmin += SQR( C[i] - Bmax[i] );     
    --         }
    --     if( dmin <= r2 ) return TRUE;


    where d = sum [ dist boxLow boxHigh coord 
                  | (boxLow, boxHigh, coord) 
                  <- [(l, r, x), (f, b, y), (t, u, z)]
                  ]
          dist bl bh v = (if v <= bl then abs (v - bl) else 0)
                         +
                         (if v >= bh then abs (v - bh) else 0)


subBoxes :: Box -> [Box]
subBoxes ((V3 l f t), (V3 r b u)) = 
    [ ((V3 l  f  t ) , (V3 r' b' u')) 
    , ((V3 l  f  t'),  (V3 r' b' u ))
    , ((V3 l  f' t ),  (V3 r' b  u'))
    , ((V3 l  f' t'),  (V3 r' b  u ))
    , ((V3 l' f  t ) , (V3 r  b' u')) 
    , ((V3 l' f  t'),  (V3 r  b' u ))
    , ((V3 l' f' t ),  (V3 r  b  u'))
    , ((V3 l' f' t'),  (V3 r  b  u ))
    ]
    where w = (r - l) `div` 2
          r' = l + w
          b' = f + w
          u' = t + w
          l' = r' + 1
          f' = b' + 1
          t' = u' + 1


unitBox :: Box -> Bool
unitBox ((V3 l f t), (V3 r b u)) = l == r && f == b && t == u


boundingBox swarm = ((V3 minX minY minZ), (V3 maxX maxY maxZ))
    where minX = minimum $ [ _x bot | bot <- M.keys swarm ]
          minY = minimum $ [ _y bot | bot <- M.keys swarm ]
          minZ = minimum $ [ _z bot | bot <- M.keys swarm ]
          maxX = maximum $ [ _x bot | bot <- M.keys swarm ]
          maxY = maximum $ [ _y bot | bot <- M.keys swarm ]
          maxZ = maximum $ [ _z bot | bot <- M.keys swarm ]
          _x (V3 x _ _) = x
          _y (V3 _ y _) = y
          _z (V3 _ _ z) = z


part2 = distanceFromOrigin . bestUnitBox

bestUnitBox :: BotSwarm -> Box
bestUnitBox swarm = findBestBox swarm initialQueue
    where initialBox = boundingBox swarm
          initialQueue = P.singleton $ enLabel swarm initialBox

findBestBox :: BotSwarm -> BoxQueue -> Box
findBestBox swarm queue 
    | unitBox currentBox = currentBox
    | otherwise = findBestBox swarm newQueue
    where (currentLBox, queue') = P.deleteFindMax queue
          currentBox = _box currentLBox
          nextBoxes = subBoxes currentBox
          nextLBoxes = map (enLabel swarm) nextBoxes
          newQueue = foldl' (\ q b -> P.insert b q) queue' nextLBoxes

enLabel :: BotSwarm -> Box -> LabelledBox
enLabel swarm box = LabelledBox { _box = box, _intersectCount = boxIntersectionCount box swarm}


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
signedInteger = L.signed sc integer
symb = L.symbol sc
comma = symb ","
posOpenP = symb "pos=<"
posCloseP = symb ">"
radiusStartP = symb "r="

swarmP = many nanobotP
nanobotP = (,) <$> posP <* comma <*> radiusP

posP = posify <$> (posOpenP `between` posCloseP) coordP
    where posify (a, b, c) = V3 a b c
coordP = (,,) <$> signedInteger <* comma <*> signedInteger <* comma <*> signedInteger
radiusP = radiusStartP *> signedInteger


successfulParse :: Text -> [(Coord, Integer)]
-- successfulParse _ = []
successfulParse input = 
        case parse swarmP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right swarm  -> swarm
