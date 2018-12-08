{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Char (ord)

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Job = Char
type Link = (Job, Job)
type Preconditions = S.Set Job
type Schedule = M.Map Job Preconditions
data Worker = Idle | BusyUntil Job Int deriving (Show, Eq)

workerJob (BusyUntil job _) = job
workerJob Idle = '\xff'

workerFinishTime (BusyUntil _ time) = time
workerFinishTime Idle = 100000

main :: IO ()
main = do 
        text <- TIO.readFile "data/advent07.txt"
        let links = successfulParse text
        -- print links
        let schedule = buildSchedule links
        -- print schedule
        -- print $ candidates schedule
        putStrLn $ part1 schedule
        print $ part2 schedule


part1 schedule = unfoldr jobStep schedule  

part2 schedule = last $ unfoldr jobStepTimed (schedule, initialWorkers)
    where idleWorkers = take 5 $ repeat Idle
          initialWorkers = employWorkers idleWorkers 0 schedule


ensureKnown :: Job -> Schedule -> Schedule
ensureKnown j s
    | j `M.member` s = s
    | otherwise      = M.insert j S.empty s

includeLink :: Schedule -> Link -> Schedule
includeLink schedule (pre, post) = M.insert post conditions' schedule'' 
    where schedule' = ensureKnown pre schedule
          schedule'' = ensureKnown post schedule'
          conditions = schedule''!post
          conditions' = S.insert pre conditions

buildSchedule :: [Link] -> Schedule
buildSchedule = foldl' includeLink M.empty

candidates :: Schedule -> Schedule
candidates = M.filter S.null

currentJob :: Schedule -> Job
currentJob = head . availableJobs

availableJobs :: Schedule -> [Job] -- note that this sorts the keys for us
availableJobs = M.keys . candidates

performJob :: Job -> Schedule -> Schedule
performJob job schedule = schedule''
    where schedule' = M.delete job schedule 
          schedule'' = M.map (\p -> S.delete job p) schedule'

jobStep :: Schedule -> Maybe (Job, Schedule)
jobStep schedule 
    | M.null schedule = Nothing
    | otherwise = Just (job, schedule')
    where job = currentJob schedule
          schedule' = performJob job schedule


jobDuration :: Job -> Int
jobDuration job = 61 + ord(job) - ord('A')
-- jobDuration job = 1 + ord(job) - ord('A')


startTimedJob :: Job -> Int -> Worker
startTimedJob job startTime = BusyUntil job (startTime + jobDuration job)


employWorkers :: [Worker] -> Int -> Schedule -> [Worker]
employWorkers workers time schedule = take (length workers) (busyWorkers ++ newWorkers ++ repeat Idle)
    where idleWorkerCount = length $ filter (== Idle) workers
          busyWorkers = filter (/= Idle) workers
          currentJobs = map workerJob busyWorkers
          startingJobs = take idleWorkerCount $ filter (\j -> j `notElem` currentJobs) $ availableJobs schedule
          newWorkers = map (\job -> startTimedJob job time) startingJobs
-- employWorkers workers _ _ = workers

completeTimedJob :: Schedule -> Job -> Schedule
completeTimedJob schedule job = schedule''
    where schedule' = M.delete job schedule 
          schedule'' = M.map (\p -> S.delete job p) schedule'


earliestFinishTime :: [Worker] -> Int
earliestFinishTime workers = minimum $ map workerFinishTime workers


finishJobs :: [Worker] -> Schedule -> ([Worker], Schedule, Int)
finishJobs workers schedule = (continuingWorkers ++ idleWorkers, schedule', time)
    where time = earliestFinishTime workers
          (finishingWorkers, continuingWorkers) = partition (\w -> workerFinishTime w == time) workers 
          schedule' = foldl' completeTimedJob schedule $ map workerJob finishingWorkers
          idleWorkers = map fst $ zip (repeat Idle) finishingWorkers


jobStepTimed :: (Schedule, [Worker]) -> Maybe (Int, (Schedule, [Worker]))
jobStepTimed (schedule, workers) 
    | M.null schedule = Nothing
    | otherwise = Just (time, (schedule', workers''))
    where (workers', schedule', time) = finishJobs workers schedule
          workers'' = employWorkers workers' time schedule'



-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty

-- lexeme  = L.lexeme sc
-- integer = lexeme L.decimal
symb = L.symbol sc

prefixP = symb "Step"
infixP = symb " must be finished before step"
suffixP = symb " can begin."

linkFileP = many linkP

-- linkP = pairify <$> prefixP <*> upperChar <* infixP <*> upperChar <* suffixP 
--     where pairify _ a b = (a, b)
linkP = (,) <$> (prefixP *> upperChar <* infixP) <*> upperChar <* suffixP 

successfulParse :: Text -> [Link]
successfulParse input = 
        case parse linkFileP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right links -> links