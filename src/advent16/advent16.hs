{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.List
import qualified Data.Set as S

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Bits ((.&.), (.|.))

type Memory = M.Map Int Int
data Operation = 
      Addr
    | Addi
    | Mulr
    | Muli
    | Banr
    | Bani
    | Borr
    | Bori
    | Setr
    | Seti
    | Gtir
    | Gtri
    | Gtrr
    | Eqir
    | Eqri
    | Eqrr
    deriving (Eq, Show, Enum, Bounded, Ord)
data RawOperation = RawOperation {_opcode :: Int, _a :: Int, _b :: Int, _c :: Int} deriving (Eq, Show)
data Test = Test {_preMemory :: Memory, _testOperation :: RawOperation, _postMemory :: Memory} deriving (Eq, Show)
type Candidates = M.Map Int (S.Set Operation)
type OpcodeAssignments = M.Map Int Operation

main :: IO ()
main = do 
    text <- TIO.readFile "data/advent16.txt"
    let (tests, program) = successfulParse text
    print $ part1 tests
    print $ part2 tests program

part1 :: [Test] -> Int
part1 = length . filter (>= 3) . map S.size . map matches

part2 :: [Test] -> [RawOperation] -> Int
part2 tests program = finalMemory!0
    where candidates = candidateOpcodes tests
          opcodes = findOpcodes candidates
          finalMemory = foldl' (\m op -> execute op opcodes m) (M.singleton 0 0) program


execute :: RawOperation -> OpcodeAssignments -> Memory -> Memory
execute op codes m = perform o (_a op) (_b op) (_c op) m
    where o = codes!(_opcode op)

perform :: Operation -> Int -> Int -> Int -> Memory -> Memory
perform Addr a b c memory = M.insert c (memory!a + memory!b) memory
perform Addi a b c memory = M.insert c (memory!a + b) memory
perform Mulr a b c memory = M.insert c (memory!a * memory!b) memory
perform Muli a b c memory = M.insert c (memory!a * b) memory
perform Banr a b c memory = M.insert c (memory!a .&. memory!b) memory
perform Bani a b c memory = M.insert c (memory!a .&. b) memory
perform Borr a b c memory = M.insert c (memory!a .|. memory!b) memory
perform Bori a b c memory = M.insert c (memory!a .|. b) memory
perform Setr a b c memory = M.insert c (memory!a) memory
perform Seti a b c memory = M.insert c a memory
perform Gtir a b c memory = M.insert c (if a > (memory!b) then 1 else 0) memory
perform Gtri a b c memory = M.insert c (if (memory!a) > b then 1 else 0) memory
perform Gtrr a b c memory = M.insert c (if (memory!a) > (memory!b) then 1 else 0) memory
perform Eqir a b c memory = M.insert c (if a == memory!b then 1 else 0) memory
perform Eqri a b c memory = M.insert c (if (memory!a) == b then 1 else 0) memory
perform Eqrr a b c memory = M.insert c (if (memory!a) == (memory!b) then 1 else 0) memory

doTest :: Test -> Operation -> Bool
doTest test operation = calculatedMemory == (_postMemory test)
    where rawOp = _testOperation test
          calculatedMemory = perform operation (_a rawOp) (_b rawOp) (_c rawOp) (_preMemory test)

matches :: Test -> S.Set Operation
matches test = S.fromList $ filter (doTest test) [minBound..maxBound]



candidateOpcodes :: [Test] -> Candidates
candidateOpcodes tests = foldl' restrict M.empty tests

restrict :: Candidates -> Test -> Candidates
restrict candidates test = M.insertWith S.intersection opcode possibles candidates
    where opcode = _opcode $ _testOperation test
          possibles = matches test

findOpcodes :: Candidates -> OpcodeAssignments
findOpcodes candidates = findOpcodes' candidates M.empty

findOpcodes' :: Candidates -> OpcodeAssignments -> OpcodeAssignments
findOpcodes' candidates assignments 
    | M.null candidates = assignments
    | otherwise = findOpcodes' candidates'' assignments'
    where singletons = M.map S.findMin $ M.filter (\c -> S.size c == 1) candidates
          assignments' = assignments `M.union` singletons
          candidates' = candidates `M.difference` singletons
          founds = S.fromList $ M.elems assignments
          candidates'' = M.map (\cs -> S.filter (\c -> c `S.notMember` founds) cs) candidates'


-- allOps :: S.Set Operation
-- allOps = S.fromList [minBound..maxBound]

-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

memoryP = (M.fromList . zip [0..] . map fromIntegral) <$> between (symb "[") (symb "]") (integer `sepBy` (symb ","))

beforeP = symb "Before:" *> memoryP
afterP = symb "After:" *> memoryP

rawOpP = opify <$> integer <*> integer <*> integer <*> integer
    where opify o a b c = RawOperation { _opcode = fromIntegral o
                                       , _a = fromIntegral a
                                       , _b = fromIntegral b
                                       , _c = fromIntegral c
                                       }

testP = testify <$> beforeP <*> rawOpP <*> afterP
    where testify b o a = Test {_preMemory = b, _testOperation = o, _postMemory = a}

fileP = (,) <$> (many testP) <*> (many rawOpP)

successfulParse :: Text -> ([Test], [RawOperation])
successfulParse input = 
        case parse fileP "input" input of
                Left  _error -> ([], []) -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right world -> world