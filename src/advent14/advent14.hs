import qualified Data.Sequence as Q
import Data.Sequence ((<|), (|>))
import Data.List
import Data.Foldable (toList)

type Recipes = Q.Seq Int
data State = State Int Int Recipes deriving (Eq, Show)

targetLength = 327901
-- targetLength = 59414

main :: IO ()
main = do 
    let state = State 0 1 (Q.fromList [3, 7])
    putStrLn $ part1 state
    print $ part2 state

part1 :: State -> String
part1 state0 = concatMap show $ toList $ Q.take 10 $ Q.drop targetLength recipes
    where (State _ _ recipes) = last $ takeWhile unfinished1 $ states state0

unfinished1 :: State -> Bool
unfinished1 (State _ _ recipes) = (Q.length recipes) <= (targetLength + 10)

part2 :: State -> Int
part2 state0 = if (takeR (Q.length targetSeq) recipes) == targetSeq
               then (Q.length recipes) - (Q.length targetSeq)
               else (Q.length recipes) - (Q.length targetSeq) - 1
-- recipes -- (Q.length recipes) - (Q.length targetSeq)

    where (State _ _ recipes) = head $ dropWhile (unfinished2 targetSeq) $ states state0

unfinished2 :: Recipes -> State -> Bool
unfinished2 target (State _ _ recipes) = 
    ((takeR (Q.length target) recipes) /= target)
    && 
    ((Q.take (Q.length target) (takeR (1 + Q.length target) recipes)) /= target)

states :: State -> [State]
states = iterate extend

extend :: State -> State
extend (State e1 e2 recipes) = State e1' e2' recipes'
    where v1 = Q.index recipes e1
          v2 = Q.index recipes e2
          total = v1 + v2
          recipes' = if total >= 10
                     then recipes |> (total `div` 10) |> (total `mod` 10)
                     else recipes |> total
          e1' = (e1 + v1 + 1) `mod` (Q.length recipes')
          e2' = (e2 + v2 + 1) `mod` (Q.length recipes')

targetSeq :: Recipes
targetSeq = Q.fromList $ map read $ map (take 1 . drop 1) $ map show $ show targetLength 

takeR n s = Q.drop (Q.length s - n) s
