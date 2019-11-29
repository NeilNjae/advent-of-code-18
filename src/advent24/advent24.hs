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


data Group = Group { _units :: Int
                   , _hps :: Int
                   , _modifiers :: [Modifier]
                   , _damage :: Int
                   , _damageType :: String
                   , _initiative :: Int
                   } deriving (Eq, Show)
instance Ord Group where
    g1 `compare` g2 = if (effectivePower g1) == (effectivePower g2)
                      then (_initiative g1) `compare` (_initiative g2)
                      else (effectivePower g1) `compare` (effectivePower g2)

data Modifier = Weakness [String] | Immunity [String]  
        deriving (Eq, Show)

type Army = M.Map Int Group
type Allocation = (Int, Int)
data BattleOrder = Immune Int | Infection Int deriving (Eq, Show)


main :: IO ()
main = do 
        text <- TIO.readFile "data/advent24.txt"
        let armies = successfulParse text
        print armies
        print $ part1 (fst armies) (snd armies)
        print $ part2 (fst armies) (snd armies)


part1 :: Army -> Army -> Int
part1 immuneArmy infectionArmy = uncurry remainingUnitCount endState
    where endState = battle immuneArmy infectionArmy


part2 immuneArmy infectionArmy = (minimalBoost, part1 immuneArmy' infectionArmy)
    where boostUpper = findSuccessfulBoost 1 immuneArmy infectionArmy
          minimalBoost = boostSearch 0 boostUpper immuneArmy infectionArmy
          immuneArmy' = applyBoost minimalBoost immuneArmy


findSuccessfulBoost boost immuneArmy infectionArmy = 
    if success 
    then boost
    else findSuccessfulBoost (2 * boost) immuneArmy infectionArmy
    where success = immuneSuccessWithBoost boost immuneArmy infectionArmy

boostSearch lower upper _ _ | trace ("Searching in " ++ show (lower, upper)) False = undefined
boostSearch lower upper immuneArmy infectionArmy = 
    if lower == upper
    then lower
    else boostSearch lower' upper' immuneArmy infectionArmy
    where boost = lower + (upper - lower) `div` 2
          success = immuneSuccessWithBoost boost immuneArmy infectionArmy
          lower' = if success then lower else boost + 1
          upper' = if success then boost else upper

immuneSuccessWithBoost :: Int -> Army -> Army -> Bool
immuneSuccessWithBoost boost _ _ | trace ("Trial with boost " ++ show boost) False = undefined
immuneSuccessWithBoost boost immuneArmy infectionArmy = immuneWins $ battle immuneArmy' infectionArmy
    where immuneArmy' = applyBoost boost immuneArmy



effectivePower :: Group -> Int
effectivePower group = (_units group) * (_damage group)

damageCaused :: Group -> Group -> Int
damageCaused attacker defender = 
    if attackType `elem` immunities then 0
    else if attackType `elem` weaknesses then (2 * effectivePower attacker)
         else effectivePower attacker
    where attackType = _damageType attacker
          weaknesses = foldl' extractWeakness [] (_modifiers defender)
          immunities = foldl' extractImmunity [] (_modifiers defender)

extractWeakness :: [String] -> Modifier -> [String]
extractWeakness currentWeaknesses (Weakness ws) = currentWeaknesses ++ ws
extractWeakness currentWeaknesses (Immunity _ws) = currentWeaknesses

extractImmunity :: [String] -> Modifier -> [String]
extractImmunity currentImmunity (Weakness _ms) = currentImmunity
extractImmunity currentImmunity (Immunity ms) = currentImmunity ++ ms

applyDamage :: Group -> Int -> Group
applyDamage group damage = group { _units = unitsRemaining }
    where unitsKilled = damage `div` (_hps group)
          unitsRemaining = maximum [0, (_units group) - unitsKilled]


keysByEffectivePower :: Army -> [Int]
keysByEffectivePower army = reverse $ sortOn (\k -> effectivePower (army!k)) (M.keys army)

allocateAttackers :: Army -> Army -> [Allocation]
allocateAttackers attackers defenders = fst $ foldl' (allocateAttacker attackers defenders) ([], M.keys defenders) $ keysByEffectivePower attackers

allocateAttacker :: Army -> Army -> ([Allocation], [Int]) -> Int -> ([Allocation], [Int])
-- allocateAttacker attackers defenders allocated@(assignedTargets, availableTargets) attackerKey | trace ("Allocate " ++ show attackerKey ++ "\n" ++ show allocated ++ "\n" ++ show [(t, sortTarget t) | t <- targets]) False = undefined
--     where targets = reverse $ sortOn sortTarget availableTargets
--           sortTarget t = ( damageCaused (attackers!attackerKey) (defenders!t) 
--                          , effectivePower (defenders!t) 
--                          , _initiative (defenders!t)
--                          )
allocateAttacker attackers defenders (assignedTargets, availableTargets) attackerKey = 
    if null viableTargets 
    then (assignedTargets, availableTargets)
    else (((attackerKey, target):assignedTargets), delete target availableTargets)
    where attacker = attackers!attackerKey
          viableTargets = filter (\t -> damageCaused attacker (defenders!t) > 0 ) availableTargets
          target = head $ reverse $ sortOn sortTarget viableTargets
          sortTarget t = ( damageCaused attacker (defenders!t) 
                         , effectivePower (defenders!t) 
                         , _initiative (defenders!t)
                         )


battleOrder :: Army -> Army -> [BattleOrder]
battleOrder immuneArmy infectionArmy = mergeOrders immuneIds infectIds
    where armyIds army = reverse $ sort [(_initiative (army!k), k) | k <- M.keys army]
          immuneIds = armyIds immuneArmy
          infectIds = armyIds infectionArmy

mergeOrders :: [(Int, Int)] -> [(Int, Int)] -> [BattleOrder]
mergeOrders [] ids = [ Infection k | (_, k) <- ids ]
mergeOrders ids [] = [ Immune k | (_, k) <- ids ]
mergeOrders ((i1, k1):id1s) ((i2, k2):id2s)
    | i1 >= i2  = (Immune k1):(mergeOrders id1s ((i2, k2):id2s))
    | otherwise = (Infection k2):(mergeOrders ((i1, k1):id1s) id2s)

battleRound :: (Army, Army) -> (Army, Army)
-- battleRound (immuneArmy, infectionArmy) | trace ("Round\n" ++ show immuneArmy ++ " " ++ show infectionArmy) False = undefined
-- battleRound immuneArmy infectionArmy | trace (show (armyCount immuneArmy) ++ " " ++ show (armyCount infectionArmy)) False = undefined
battleRound (immuneArmy, infectionArmy) = (pruneArmy immuneArmy'', pruneArmy infectionArmy')
    where immuneAllocations = allocateAttackers immuneArmy infectionArmy
          infectionAllocations = allocateAttackers infectionArmy immuneArmy
          actionOrder = battleOrder immuneArmy infectionArmy
          (immuneArmy', infectionArmy') =     foldl' (\ (a1, a2) order -> handleOrder order immuneAllocations infectionAllocations a1 a2) 
                                                     (immuneArmy, infectionArmy)  actionOrder
          -- test for stalemate
          immuneArmy'' = if (immuneArmy' == immuneArmy) && (infectionArmy' == infectionArmy)
                         then M.empty
                         else immuneArmy'



handleOrder :: BattleOrder -> [Allocation] -> [Allocation] -> Army -> Army -> (Army, Army)
handleOrder (Immune k) allocations _ attackArmy defendArmy = (attackArmy, defendArmy')
    where defendArmy' = handleAttack k allocations attackArmy defendArmy
handleOrder (Infection k) _ allocations defendArmy attackArmy = (defendArmy', attackArmy)
    where defendArmy' = handleAttack k allocations attackArmy defendArmy

handleAttack :: Int -> [Allocation] -> Army -> Army -> Army
handleAttack attacker allocations attackArmy defendArmy = 
    if not $ null attackersAllocations
    then M.insert defender defendGroup' defendArmy
    else defendArmy
    where attackersAllocations = filter (\a -> attacker == fst a ) allocations
          defender = snd $ head attackersAllocations
          defendGroup = (defendArmy!defender)
          damage = damageCaused (attackArmy!attacker) defendGroup
          defendGroup' = applyDamage defendGroup damage


battle :: Army -> Army -> (Army, Army)
battle immuneArmy infectionArmy = head $ dropWhile (not . uncurry battleOver) $ iterate battleRound (immuneArmy, infectionArmy)


pruneArmy :: Army -> Army
pruneArmy = M.filter (\g -> _units g > 0)

battleOver :: Army -> Army -> Bool
battleOver immuneArmy infectionArmy = (M.null immuneArmy) || (M.null infectionArmy)


remainingUnitCount :: Army -> Army -> Int
-- remainingUnitCount immuneArmy infectionArmy | trace ("End with\n" ++ show immuneArmy ++ " " ++ show infectionArmy) False = undefined
remainingUnitCount immuneArmy infectionArmy = (unitCount immuneArmy) + (unitCount infectionArmy)

unitCount :: Army -> Int
unitCount army = sum [_units g | g <- M.elems army]


immuneWins :: (Army, Army) -> Bool
immuneWins (immuneArmy, infectionArmy) = (unitCount immuneArmy > 0) && (unitCount infectionArmy == 0)


applyBoost :: Int -> Army -> Army
applyBoost boost = M.map (\g -> g { _damage = (_damage g + boost)})


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
-- signedInteger = L.signed sc integer
symb = L.symbol sc
comma = symb ","
semicolon = symb ";"
openBracket = symb "("
closeBracket = symb ")"

immuneHeaderP = symb "Immune System:"
infectionHeaderP = symb "Infection:"

sizePaddingP = symb "units each with"
hpPaddingP = symb "hit points"
attackPaddingP = symb "with an attack that does"
initiativePaddingP = symb "damage at initiative"
weaknessPaddingP = symb "weak to"
immunityPaddingP = symb "immune to"


armiesP = (,) <$> immuneGroupsP <*> infectionGroupsP

immuneGroupsP = immuneHeaderP *> many groupP
infectionGroupsP = infectionHeaderP *> many groupP

-- 72 units each with 5294 hit points (weak to slashing; immune to radiation, cold) with an attack that does 639 fire damage at initiative 1

groupP = engroup <$> (integer <* sizePaddingP )
                 <*> (integer <* hpPaddingP)
                 <*> (attackModifierGroupP <* attackPaddingP )
                 <*> integer
                 <*> (damageTypeP <* initiativePaddingP)
                 <*> integer
    where engroup units hps aMods damage damageType initative = 
                Group { _units = units
                      , _hps = hps
                      , _modifiers = aMods
                      , _damage = damage
                      , _damageType = damageType
                      , _initiative = initative 
                      }


attackModifierGroupP = option [] ((openBracket `between` closeBracket) attackModifiersP)

attackModifiersP = attackModifierP `sepBy` semicolon
attackModifierP = weaknessP <|> immunityP
weaknessP = Weakness <$> (weaknessPaddingP *> damageTypesP)
immunityP = Immunity <$> (immunityPaddingP *> damageTypesP)

damageTypeP = some letterChar <* sc
damageTypesP = damageTypeP `sepBy` comma

successfulParse :: Text -> (Army, Army)
-- successfulParse _ = []
successfulParse input = 
        case parse armiesP "input" input of
                Left  _error -> (M.empty, M.empty) -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right armies -> idTag armies
        where idTag (immune, infect) = (tagArmy immune, tagArmy infect)
              tagArmy army = M.fromList $ zip [1..] army
