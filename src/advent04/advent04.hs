{-# LANGUAGE OverloadedStrings #-}

import Data.List
-- import Data.Tuple (swap)

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Data.Time
-- import Data.Time.LocalTime

import qualified Data.Set as S
import qualified Data.Map.Strict as M

type GuardId = Integer

data LogEvent = Arrives GuardId | Sleeps | Wakes deriving (Eq, Show, Ord)
data LogEntry = LogEntry { _logTime :: UTCTime , _logEvent :: LogEvent } deriving (Eq, Show, Ord)

data GuardState = Asleep UTCTime | Awake
data LogTracker = LogTracker {_currentGuard :: GuardId, _currentState :: GuardState }

type GuardActivity = S.Set (UTCTime, GuardId)
type Guards = S.Set GuardId
-- type Minutes = S.Set Int
type GuardSleepDuration = M.Map GuardId Int
type SleepFrequency = M.Map Int Int -- key = minute, value = times spent asleep
type GuardSleepFrequency = M.Map (GuardId, Int) Int -- key = (guardID, minute), value = times spent asleep



main :: IO ()
main = do 
        text <- TIO.readFile "data/advent04.txt"
        let guardLog = sort $ successfulParse text
        let activity = buildActivity guardLog
        print $ part1 activity
        print $ part2 activity

part1 :: GuardActivity -> Int
part1 activity = (fromIntegral sg) * mostAsleep
    where sd = sleepDurations activity
          sg = sleepiestGuard sd
          sga = guardActivity sg activity
          sgf = sleepFrequency sga
          mostAsleep = keyOfMaxValue sgf

part2 :: GuardActivity -> Int
part2 activity = (fromIntegral g) * m
    where gids = guardsOf activity
          sleepTimes = M.fromSet (\gid -> guardSleepFrequency gid activity) gids
          gsfs = M.foldrWithKey' rekeySleep M.empty sleepTimes
          (g, m) = keyOfMaxValue gsfs

rekeySleep :: GuardId -> SleepFrequency -> GuardSleepFrequency -> GuardSleepFrequency
rekeySleep gid sleepFreq guardSleepFreq = M.foldrWithKey' (\m f gsf -> M.insert (gid, m) f gsf) guardSleepFreq sleepFreq



buildActivity :: [LogEntry] -> GuardActivity
buildActivity guardLog = snd $ foldl' processLogEntry' (initialTracker, S.empty) guardLog
    where initialTracker = LogTracker {_currentGuard = 0, _currentState = Awake}
          processLogEntry' (tracker, activity) entry = processLogEntry (_logEvent entry) (_logTime entry) tracker activity

processLogEntry :: LogEvent -> UTCTime -> LogTracker -> GuardActivity -> (LogTracker, GuardActivity)
processLogEntry (Arrives gid) _    _       activity = (LogTracker {_currentGuard = gid, _currentState = Awake}, activity)
processLogEntry Sleeps        time tracker activity = (tracker {_currentState = Asleep time}, activity)
processLogEntry Wakes         time tracker activity = (tracker {_currentState = Awake}, activity')
    where Asleep sleepTime = _currentState tracker
          guardId = _currentGuard tracker
          sleepMinutes = unfoldr unfoldF sleepTime 
          unfoldF now = if now >= time then Nothing
                        else Just ((now, guardId), addUTCTime 60 now)
          activity' = S.union activity $ S.fromList sleepMinutes

guardsOf :: GuardActivity -> Guards
guardsOf = S.map snd

-- minutesOf :: GuardActivity -> Minutes
-- minutesOf activity = S.map (toMinutes . fst) activity

toMinutes :: UTCTime -> Int
toMinutes = todMin . timeToTimeOfDay . utctDayTime

totalSleepDuration :: GuardId -> GuardActivity -> Int
totalSleepDuration gid activity = S.size $ guardActivity gid activity

-- all activity of one guard
guardActivity :: GuardId -> GuardActivity -> GuardActivity
guardActivity gid activity = S.filter (\(_, g) -> g == gid) activity

sleepDurations :: GuardActivity -> GuardSleepDuration
sleepDurations activity = M.fromSet guardSleepDuration gids 
    where gids = guardsOf activity
          guardSleepDuration gid = totalSleepDuration gid activity


sleepiestGuard :: GuardSleepDuration -> GuardId
sleepiestGuard = keyOfMaxValue


keyOfMaxValue :: Ord b => M.Map a b -> a
keyOfMaxValue m = fst $ M.foldrWithKey mergeKV (M.findMin m) m
    where mergeKV k v (bestK, bestV) = 
            if v > bestV then (k, v) else (bestK, bestV)


sleepFrequency :: GuardActivity -> SleepFrequency
sleepFrequency activity = S.foldl' updateSF M.empty activity
    where updateSF m (t, _) = M.insert (toMinutes t) ((M.findWithDefault 0 (toMinutes t) m) + 1) m

guardSleepFrequency :: GuardId -> GuardActivity -> SleepFrequency
guardSleepFrequency gid activity = sleepFrequency $ guardActivity gid activity


-- Parse the input file

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

openP = symb "["
closeP = symb "]"
dashP = symb "-"
colonP = symb ":"


logFileP = many logEntryP
logEntryP = logify <$> timeStampP <*> eventP
    where logify t e = LogEntry {_logTime = t, _logEvent = e}


eventP = arrivesP <|> sleepsP <|> wakesP
arrivesP = Arrives <$> ((symb "Guard #") *> integer <* (symb "begins shift"))
sleepsP = Sleeps <$ (symb "falls asleep")
wakesP = Wakes <$ (symb "wakes up")

-- [1518-10-25 00:48]
timeStampP = between openP closeP timeStampInnerP
timeStampInnerP = dtify <$> integer <* dashP <*> integer <* dashP <*> integer <*> integer <* colonP <*> integer
    where dtify y mo d h mi = UTCTime (fromGregorian y (fromIntegral mo) (fromIntegral d)) (tify h mi)
          tify h mi = secondsToDiffTime ((h * 60) + mi) * 60

successfulParse :: Text -> [LogEntry]
successfulParse input = 
        case parse logFileP "input" input of
                Left  _error -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right guardLog -> guardLog