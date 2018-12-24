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

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Data.Bits ((.&.), (.|.))

import Control.Monad (when)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer

type Memory = M.Map Integer Integer

data Location = Literal Integer | Register Integer deriving (Show, Eq)
data Instruction = 
      Addr Integer Integer Integer
    | Addi Integer Integer Integer
    | Mulr Integer Integer Integer
    | Muli Integer Integer Integer
    | Banr Integer Integer Integer
    | Bani Integer Integer Integer
    | Borr Integer Integer Integer
    | Bori Integer Integer Integer
    | Setr Integer Integer Integer
    | Seti Integer Integer Integer
    | Gtir Integer Integer Integer
    | Gtri Integer Integer Integer
    | Gtrr Integer Integer Integer
    | Eqir Integer Integer Integer
    | Eqri Integer Integer Integer
    | Eqrr Integer Integer Integer
    deriving (Eq, Show, Ord)


data Machine = Machine { _registers :: M.Map Integer Integer
                       , _pc :: Int
                       -- , _pcReg :: Integer
                       } 
               deriving (Show, Eq)

type ProgrammedMachine = WriterT [Integer] (ReaderT (Integer, [Instruction]) (State Machine)) ()

emptyMachine = Machine {_registers = M.fromList (zip [0..5] (repeat 0)), 
                        _pc = 0}

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent19.txt"
        let (ip, instrs) = successfulParse text
        print (ip, instrs)
        -- print $ part1 ip instrs
        print $ sum [i | i <- [1..1032], 1032 `mod` i == 0]
        -- print $ part2 ip instrs
        print $ sum [i | i <- [1..10551432], 10551432 `mod` i == 0]

part1 ip instructions = 
    runState (
        runReaderT (
            runWriterT executeInstructions
                   ) 
            (ip, instructions)
             ) 
             emptyMachine

part2 ip instructions = 
    runState (
        runReaderT (
            runWriterT executeInstructions
                   ) 
            (ip, instructions)
             ) 
             m2
    where emptyRegisters = _registers emptyMachine
          m2 = emptyMachine {_registers = M.insert 0 1 emptyRegisters}

executeInstructions = 
    do  (_, instrs) <- ask
        m <- get
        when (_pc m >= 0 && _pc m < length instrs)
            $
            do executeInstruction
               executeInstructions

executeInstruction :: ProgrammedMachine
executeInstruction =
    do  (pcIs, instrs) <- ask
        m <- get
        let instr = instrs!!(_pc m)
        let memory0 = _registers m
        let memory1 = M.insert pcIs (fromIntegral (_pc m)) memory0
        let memory2 = if canPeep1 instrs (_pc m) memory1 --  sample1 == sample1Target
                      then memoryPeep1 memory1
                      else perform instr memory1 
        -- let memory2 = perform instr memory1 
        let pc' = fromIntegral ((memory2!pcIs) + 1)
        -- let aaa = trace ("pc: " ++ show (_pc m) ++ " m0: " ++ show memory0 ++ " m1: " ++ show memory1 ++ "m2: " ++ show memory2 ++ "pc': " ++ show pc') $! True
        let m' = m {_registers = memory2, _pc = pc'}
        put m'
    where sample1 pcVal instructions = take (length sample1Target) $ drop pcVal instructions
          sample1Target = [ Mulr 3 1 4
                          , Eqrr 4 2 4
                          , Addr 4 5 5
                          , Addi 5 1 5
                          , Addr 3 0 0
                          , Addi 1 1 1
                          , Gtrr 1 2 4
                          , Addr 5 4 5
                          , Seti 2 7 5
                          ]
          canPeep1 instructions pcVal mem = False -- ((sample1 pcVal instructions) == sample1Target) && ((mem!4) == 0)
          memoryPeep1 mem = M.union (M.fromList [(0, mem!0 + (if (((mem!2) `mod` (mem!3)) == 0) then mem!3 else 0)), (1, mem!2), (4, mem!2)]) mem
          -- M.insert 0 (mem!0 + mem!3) $ M.insert 1 (mem!2) $ M.insert 4 (mem!2) mem


perform :: Instruction -> Memory -> Memory
-- perform instr memory | ((memory!5 == 7) || ((memory!5 == 3) && (memory!1 == 1))) && (trace ("Perform " ++ show instr ++ " " ++ show memory) False) = undefined
perform instr memory | trace ("Perform " ++ show instr ++ " " ++ show memory) False = undefined
perform (Addr a b c) memory = M.insert c (memory!a + memory!b) memory
perform (Addi a b c) memory = M.insert c (memory!a + b) memory
perform (Mulr a b c) memory = M.insert c (memory!a * memory!b) memory
perform (Muli a b c) memory = M.insert c (memory!a * b) memory
perform (Banr a b c) memory = M.insert c (memory!a .&. memory!b) memory
perform (Bani a b c) memory = M.insert c (memory!a .&. b) memory
perform (Borr a b c) memory = M.insert c (memory!a .|. memory!b) memory
perform (Bori a b c) memory = M.insert c (memory!a .|. b) memory
perform (Setr a b c) memory = M.insert c (memory!a) memory
perform (Seti a b c) memory = M.insert c a memory
perform (Gtir a b c) memory = M.insert c (if a > (memory!b) then 1 else 0) memory
perform (Gtri a b c) memory = M.insert c (if (memory!a) > b then 1 else 0) memory
perform (Gtrr a b c) memory = M.insert c (if (memory!a) > (memory!b) then 1 else 0) memory
perform (Eqir a b c) memory = M.insert c (if a == memory!b then 1 else 0) memory
perform (Eqri a b c) memory = M.insert c (if (memory!a) == b then 1 else 0) memory
perform (Eqrr a b c) memory = M.insert c (if (memory!a) == (memory!b) then 1 else 0) memory


-- evaluate :: Machine -> Location -> Integer
-- evaluate _ (Literal i)  = i
-- evaluate m (Register r) = M.findWithDefault 0 r (registers m)



type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc


instructionsP = (,) <$> headerP <*> many instructionP
instructionP = choice [ addrP, addiP, mulrP, muliP, banrP, baniP, 
    borrP, boriP, setrP, setiP, gtirP, gtriP, gtrrP, 
    eqirP, eqriP, eqrrP ]

headerP = symb "#ip" *> integer

addrP = Addr <$> (try (symb "addr") *> integer) <*> integer <*> integer
addiP = Addi <$> (try (symb "addi") *> integer) <*> integer <*> integer
mulrP = Mulr <$> (try (symb "mulr") *> integer) <*> integer <*> integer
muliP = Muli <$> (try (symb "muli") *> integer) <*> integer <*> integer
banrP = Banr <$> (try (symb "banr") *> integer) <*> integer <*> integer
baniP = Bani <$> (try (symb "bani") *> integer) <*> integer <*> integer
borrP = Borr <$> (try (symb "borr") *> integer) <*> integer <*> integer
boriP = Bori <$> (try (symb "bori") *> integer) <*> integer <*> integer
setrP = Setr <$> (try (symb "setr") *> integer) <*> integer <*> integer
setiP = Seti <$> (try (symb "seti") *> integer) <*> integer <*> integer
gtirP = Gtir <$> (try (symb "gtir") *> integer) <*> integer <*> integer
gtriP = Gtri <$> (try (symb "gtri") *> integer) <*> integer <*> integer
gtrrP = Gtrr <$> (try (symb "gtrr") *> integer) <*> integer <*> integer
eqirP = Eqir <$> (try (symb "eqir") *> integer) <*> integer <*> integer
eqriP = Eqri <$> (try (symb "eqri") *> integer) <*> integer <*> integer
eqrrP = Eqrr <$> (try (symb "eqrr") *> integer) <*> integer <*> integer

successfulParse :: Text -> (Integer, [Instruction])
successfulParse input = 
        case parse instructionsP "input" input of
                Left  _error -> (0, []) -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right instructions  -> instructions