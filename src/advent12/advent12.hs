{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List
import qualified Data.Set as S

type Pots = S.Set Int
data Rule = Rule [Bool] Bool deriving (Eq, Show)

main :: IO ()
main = do 
    text <- TIO.readFile "data/advent12.txt"
    let (initial, rules) = successfulParse text
    let row = makeWorld 0 initial
    print $ part1 rules row
    print $ part2 rules row

part1 :: [Rule] -> Pots -> Int
part1 rules row = sum $ (iterate (\r -> applyRules rules r) row)!!20


part2 :: [Rule] -> Pots -> Integer
part2 rules pots = (fromIntegral (sum lc)) + steadyDiff * remainingGenerations
    where rows = (iterate (\r -> applyRules rules r) pots)
          rowQuads = zip4 rows (drop 1 rows) (drop 2 rows) (drop 3 rows)
          sameDiffs (a, b, c, d) = length (nub [(sum a) - (sum b), (sum b) - (sum c), (sum c) - (sum d) ]) == 1
          differentQuads = takeWhile (not . sameDiffs) rowQuads
          (_la, _lb, lc, ld) = last differentQuads
          remainingGenerations = 50000000000 - (fromIntegral (length differentQuads)) - 1
          steadyDiff = fromIntegral $ (sum ld) - (sum lc)


makeWorld :: Int -> [Bool] -> Pots
makeWorld start = S.fromList . map fst . filter snd . zip [start..]

applyRuleAt :: [Rule] -> Int -> Pots -> (Int, Bool)
applyRuleAt rules location pots = (location, result)
    where (Rule _ result) = head $ filter (\r -> matchRuleAt r location pots) rules

matchRuleAt :: Rule -> Int -> Pots -> Bool
matchRuleAt (Rule pattern _) location pots = patternHere == potsHere
    where patternHere = makeWorld (location - 2) pattern
          potsHere = S.filter (\l -> abs (location - l) <= 2) pots 


applyRules :: [Rule] -> Pots -> Pots
applyRules rules pots = S.fromList $ map fst $ filter snd potValues
    where start = S.findMin pots
          end = S.findMax pots
          potValues = map (\location -> applyRuleAt rules location pots) [(start-3)..(end+3)]

-- showPots pots = map (\i -> showPot i pots) [-10..110]
--     where showPot i pots = if i `S.member` pots then '#' else '.'


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