{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}

-- Heavily based on https://gist.github.com/gatlin/21d669321c617836a317693aef63a3c3

-- import Data.List

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Control.Applicative
import Control.Comonad

import Control.Monad (forM_)

-- import qualified Data.Set as S


-- main :: IO ()
-- main = do 
--         text <- TIO.readFile "data/advent12.txt"
--         let (initial, rules) = successfulParse text
--         print initial
--         print rules


main :: IO ()
main = do
    let ig = tape X initialGame
    let tl = timeline conwayRules ig
    forM_ (takeS 5 tl) $ \s -> do
        printSheet 3 s
        putStrLn "---"


-- These two typeclasses probably make some people groan.
class LeftRight t where
    left :: t a -> t a
    right :: t a -> t a

-- | An infinite list of values
data Stream a = a :> Stream a
    deriving (Functor)


-- * Stream utilities

-- | Allows finitary beings to view slices of a 'Stream'
takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (x :> xs) = x : (takeS (n-1) xs)

-- | Build a 'Stream' from a generator function
unfoldS :: (b -> (a, b)) -> b -> Stream a
unfoldS f c = x :> unfoldS f d
    where (x, d) = f c

-- | Build a 'Stream' by mindlessly repeating a value
repeatS :: a -> Stream a
repeatS x = x :> repeatS x

-- | Build a 'Stream' from a finite list, filling in the rest with a designated
-- default value
fromS :: a -> [a] -> Stream a
fromS def [] = repeatS def
fromS def (x:xs) = x :> (fromS def xs)

-- | Prepend values from a list to an existing 'Stream'
prependList :: [a] -> Stream a -> Stream a
prependList [] str = str
prependList (x:xs) str = x :> (prependList xs str)

-- | A 'Stream' is a comonad
instance Comonad Stream where
    extract (a :> _) = a
    duplicate s@(a :> as) = s :> (duplicate as)

-- | It is also a 'ComonadApply'
instance ComonadApply Stream where
    (f :> fs) <@> (x :> xs) = (f x) :> (fs <@> xs)

-- * Tapes, our workhorse

-- | A 'Tape' is always focused on one value in a 1-dimensional infinite stream
data Tape a = Tape (Stream a) a (Stream a)
    deriving (Functor)

-- | We can go left and right along a tape!
instance LeftRight Tape where
    left (Tape (l :> ls) c rs) = Tape ls l (c :> rs)
    right (Tape ls c (r :> rs)) = Tape (c :> ls) r rs

-- | Build out the left, middle, and right parts of a 'Tape' with generator
-- functions.
unfoldT
    :: (c -> (a, c))
    -> (c -> a)
    -> (c -> (a, c))
    -> c
    -> Tape a
unfoldT prev center next =
    Tape
    <$> unfoldS prev
    <*> center
    <*> unfoldS next

-- | A simplified unfolding mechanism that will be useful shortly
tapeIterate
    :: (a -> a)
    -> (a -> a)
    -> a
    -> Tape a
tapeIterate prev next = unfoldT (dup . prev) id (dup . next)
    where dup a = (a, a)

-- | Create a 'Tape' from a list where everything to the left is some default
-- value and the list is the focused value and everything to the right.
tapeFromList :: a -> [a] -> Tape a
tapeFromList def xs = right $ Tape background def $ fromS def xs
    where background = repeatS def

tapeToList :: Int -> Tape a -> [a]
tapeToList n (Tape ls x rs) =
    reverse (takeS n ls) ++ [x] ++ (takeS n rs)

instance Comonad Tape where
    extract (Tape _ c _) = c
    duplicate = tapeIterate left right

instance ComonadApply Tape where
    (Tape fl fc fr) <@> (Tape xl xc xr) =
        Tape (fl <@> xl) (fc xc) (fr <@> xr)


-- | Construct an infinite tape in all directions from a base value.
background :: a -> Tape a
background x = Tape (repeatS x) x (repeatS x)

-- | This just makes some things more readable
(&) :: a -> (a -> b) -> b
(&) = flip ($)

-- | A cell can be alive ('O') or dead ('X').
data Cell = X | O deriving (Eq)

instance Show Cell where
    show X = "\x2591"
    show O = "\x2593"

-- | The first list is of numbers of live neighbors to make a dead 'Cell' become
-- alive; the second is numbers of live neighbors that keeps a live 'Cell'
-- alive.
type Ruleset = ([Int], [Int])

cellToInt :: Cell -> Int
cellToInt X = 0
cellToInt O = 1

conwayRules :: Ruleset
conwayRules = ([3], [2, 3])

executeRules :: Ruleset -> Tape Cell -> Cell
executeRules rules s = go (extract s) where

    -- there is probably a more elegant way of getting all 8 neighbors
    neighbors = [ s & left & left & extract
                , s & left & extract
                , s & extract
                , s & right & extract
                , s & right & right & extract
                ]
    liveCount = sum $ map cellToInt neighbors
    go O | liveCount `elem` persist = O
         | otherwise                = X
    go X | liveCount `elem` born    = O
         | otherwise                = X

-- | A simple glider
initialGame :: [Cell]
initialGame = map cToCell "#..#.#..##......###...###"
    where cToCell c = if c == '#' then O else X

-- | A 'Stream' of Conway games.
timeline :: Ruleset -> Tape Cell -> Stream (Tape Cell)
timeline rules ig = go ig where
    go ig = ig :> (go (rules' <@> (duplicate ig)))
    rules' = background $ executeRules rules


applyRule rule cells = 


-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

symb = L.symbol sc
potP = char '.' <|> char '#'

initialPrefix = symb "initial state:"
ruleSepP = symb "=>"

fileP = (,) <$> initialP <*> many ruleP
initialP = initialPrefix *> many potP <* sc
ruleP = (,) <$> ruleLHS <* ruleSepP <*> ruleRHS
ruleLHS = count 5 potP <* sc
ruleRHS = potP <* sc

successfulParse :: Text -> (String, [(String, Char)])
successfulParse input = 
        case parse fileP "input" input of
                Left  _error -> ("", []) -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right world -> world