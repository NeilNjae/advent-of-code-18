{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

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
import qualified Data.Set as S
import Data.Bits ((.&.), (.|.))

import Control.Monad (when)
import Control.Monad.State.Strict
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
                       , _history :: S.Set Integer
                       , _previous :: Integer
                       -- , _pcReg :: Integer
                       } 
               deriving (Show, Eq)

type ProgrammedMachine = WriterT [Integer] (ReaderT (Integer, [Instruction]) (State Machine)) ()

emptyMachine = Machine { _registers = M.fromList (zip [0..5] (repeat 0))
                       , _pc = 0
                       , _history = S.empty
                       , _previous = 0
                       }

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent21.txt"
        let (ip, instrs) = successfulParse text
        -- print (ip, instrs)
        -- print $ zip [0..] instrs
        print $ part1 ip instrs
        print $ part2 ip instrs

part1 ip instructions = 
    runState (
        runReaderT (
            runWriterT executeInstructions1
                   ) 
            (ip, instructions)
             ) 
             emptyMachine

part2 ip instructions = 
    runState (
        runReaderT (
            runWriterT executeInstructions2
                   ) 
            (ip, instructions)
             ) 
             emptyMachine


-- part2 ip instructions = head (dropWhile terminates [11592302..]) - 1
-- part2 ip instructions = terminates 11592301
--     where emptyRegisters = _registers emptyMachine
--           m2 reg0 = emptyMachine {_registers = M.insert 0 reg0 emptyRegisters}
--           terminates reg0 = null $ runResult (m2 reg0) ip instructions

-- runResult machine ip instructions = r1Repeat
--     where 
--         r1Repeat = snd $ fst $ result
--         result = 
--           runState (
--             runReaderT (
--                 runWriterT executeInstructions2
--                        ) 
--                 (ip, instructions)
--                  ) 
--                  machine


executeInstructions1 = 
    do  (_, instrs) <- ask
        m <- get
        if (_pc m == 28) then do
          tell [(_registers m)!1]
        else do
          when (_pc m >= 0 && _pc m < length instrs)
              $
              do executeInstruction
                 executeInstructions1

executeInstructions2 = 
    do  (_, instrs) <- ask
        m0 <- get
        let r1 = (trace ("R1 = " ++ (show $ (_registers m0)!1) ++ " :: " ++ (show $ S.size (_history m0)))) $ (_registers m0)!1
        if (_pc m0 == 28 && (S.member r1 (_history m0))) then do
            -- abort as found a loop
            tell $ [_previous m0]
        else do
            when (_pc m0 == 28) 
                $
                do 
                    let m0' = m0 { _history = S.insert ((_registers m0)!1) (_history m0)
                                 , _previous = (_registers m0)!1 }
                    -- let x = trace ("PC = 28, register 1 = " ++ (show ((_registers m0)!1))) $! True
                    put m0'
            m <- get
            when (_pc m >= 0 && _pc m < length instrs)
                $
                do  executeInstruction
                    executeInstructions2


executeInstruction :: ProgrammedMachine
executeInstruction =
    do  (pcIs, instrs) <- ask
        m <- get
        let instr = instrs!!(_pc m)
        let memory0 = _registers m
        let memory1 = M.insert pcIs (fromIntegral (_pc m)) memory0
        let memory2 = perform instr memory1 
        let pc' = fromIntegral ((memory2!pcIs) + 1)
        -- let aaa = trace ("pc: " ++ show (_pc m) ++ " m0: " ++ show memory0 ++ " m1: " ++ show memory1 ++ "m2: " ++ show memory2 ++ "pc': " ++ show pc') $! True
        let m' = m {_registers = memory2, _pc = pc'}
        put m'


perform :: Instruction -> Memory -> Memory
-- perform instr memory | ((memory!5 == 7) || ((memory!5 == 3) && (memory!1 == 1))) && (trace ("Perform " ++ show instr ++ " " ++ show memory) False) = undefined
-- perform instr memory | trace ("Perform " ++ show instr ++ " " ++ show memory) False = undefined
perform (Addr a b c) !memory = M.insert c (memory!a + memory!b) memory
perform (Addi a b c) !memory = M.insert c (memory!a + b) memory
perform (Mulr a b c) !memory = M.insert c (memory!a * memory!b) memory
perform (Muli a b c) !memory = M.insert c (memory!a * b) memory
perform (Banr a b c) !memory = M.insert c (memory!a .&. memory!b) memory
perform (Bani a b c) !memory = M.insert c (memory!a .&. b) memory
perform (Borr a b c) !memory = M.insert c (memory!a .|. memory!b) memory
perform (Bori a b c) !memory = M.insert c (memory!a .|. b) memory
perform (Setr a b c) !memory = M.insert c (memory!a) memory
perform (Seti a b c) !memory = M.insert c a memory
perform (Gtir a b c) !memory = M.insert c (if a > (memory!b) then 1 else 0) memory
perform (Gtri a b c) !memory = M.insert c (if (memory!a) > b then 1 else 0) memory
perform (Gtrr a b c) !memory = M.insert c (if (memory!a) > (memory!b) then 1 else 0) memory
perform (Eqir a b c) !memory = M.insert c (if a == memory!b then 1 else 0) memory
perform (Eqri a b c) !memory = M.insert c (if (memory!a) == b then 1 else 0) memory
perform (Eqrr a b c) !memory = M.insert c (if (memory!a) == (memory!b) then 1 else 0) memory


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