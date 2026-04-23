module Timesheet
    ( LogEntry(..)
    , DateTime(..)
    , Log
    , renderTemplate
    , template
    , groupByWeek
    , partitionByWeek
    , getCurrentWeekStart
    , getWeekStart
    , formatWeekDate
    , renderWorkLog
    , writeWeekFiles
    , base32chars
    , randomInts
    , chunksList
    , toDay
    , logMinutes
    , entryMinutes
    ) where

import Data.Attoparsec.Text.Lazy
import qualified Data.Attoparsec.Text.Lazy as DAT
import Control.Applicative
import System.Random
import Data.Text.Lazy hiding (map, concatMap, foldl', length, show)
import Data.List
import Prelude hiding ((++), map, concatMap, unlines, length)
import qualified Data.Text.Lazy.IO
import Text.Printf
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import System.Directory (createDirectoryIfMissing)
import qualified Data.Map.Strict as Map
import qualified Data.Text

type Log = [LogEntry]

data LogEntry = LogEntry {
    comment :: Text,
    times :: [(DateTime, DateTime)],
    originalLines :: [Text]
} deriving (Show, Eq)

data DateTime = DateTime {
    day :: Text,
    month :: Text,
    date :: Int,
    hour :: Int,
    minute :: Int,
    second :: Int,
    timezone :: Text,
    year :: Int
} deriving (Show, Eq)

template :: Log -> Text -> String
template log id = Data.List.unlines [
    renderLog log,
    renderTotalHours mins id ]
    where mins = logMinutes log

logMinutes :: Log -> Int
logMinutes logs = foldl' (+) 0 $ map entryMinutes logs

entryMinutes :: LogEntry -> Int
entryMinutes (LogEntry _ dts _) = foldl' (+) 0 $ map intervalMinutes dts

-- Convert DateTime to Day for week calculations
toDay :: DateTime -> Day
toDay dt = fromGregorian (fromIntegral $ year dt) (monthToInt $ month dt) (date dt)
    where monthToInt m = case unpack m of
            "Jan" -> 1
            "Feb" -> 2
            "Mar" -> 3
            "Apr" -> 4
            "May" -> 5
            "Jun" -> 6
            "Jul" -> 7
            "Aug" -> 8
            "Sep" -> 9
            "Oct" -> 10
            "Nov" -> 11
            "Dec" -> 12
            _ -> 1

-- Get the Monday of the week for a given DateTime
getWeekStart :: DateTime -> Day
getWeekStart dt = fromWeekDate y w 1
    where day = toDay dt
          (y, w, _) = toWeekDate day

-- Group log entries by week
groupByWeek :: Log -> Map.Map Day Log
groupByWeek entries = foldl' addEntry Map.empty entries
    where addEntry acc entry@(LogEntry _ times _) =
            case times of
                [] -> acc
                ((t1, _):_) -> Map.insertWith (\new old -> old ++ new) weekStart [entry] acc
                    where weekStart = getWeekStart t1

-- Format week identifier for directory name (YYYY-MM-DD format)
formatWeekDate :: Day -> String
formatWeekDate day = printf "%04d-%02d-%02d" y m d
    where (y, m, d) = toGregorian day

getCurrentWeekStart :: Day -> Day
getCurrentWeekStart day = fromWeekDate y w 1
    where (y, w, _) = toWeekDate day

partitionByWeek :: Day -> Log -> (Log, Log)
partitionByWeek currentWeekStart entries =
    Data.List.partition isCurrentWeek entries
    where isCurrentWeek (LogEntry _ times _) = case times of
            [] -> True  -- Keep incomplete entries in current week
            ((t1, _):_) -> getWeekStart t1 == currentWeekStart

writeWeekFiles :: [Int] -> Text -> Day -> Log -> IO ()
writeWeekFiles ints logFileContent weekStart weekLog = do
    let weekId = Data.Text.Lazy.take 8 $ base32chars ints
        dirName = formatWeekDate weekStart ++ "_" ++ unpack weekId
        csvContent = template weekLog weekId
        workLogContent = renderWorkLog weekLog

    createDirectoryIfMissing True dirName
    writeFile (dirName ++ "/timesheet.csv") csvContent
    Data.Text.Lazy.IO.writeFile (dirName ++ "/work.log") workLogContent
    putStrLn $ "Created: " ++ dirName

chunksList :: Int -> [a] -> [[a]]
chunksList _ [] = []
chunksList n xs = Data.List.take n xs : chunksList n (Data.List.drop n xs)

-- Extract work.log entries for a specific week
renderWorkLog :: Log -> Text
renderWorkLog weekLog = Data.Text.Lazy.intercalate (pack "\n\n") entryTexts `append` pack "\n"
    where entryTexts = map extractEntry weekLog
          extractEntry (LogEntry comment _ originalLines) =
            Data.Text.Lazy.intercalate (pack "\n") (comment : originalLines)

renderTemplate :: Text -> Either String Log
renderTemplate logFile = eitherResult $ parse logParser logFile

intervalMinutes :: (DateTime, DateTime) -> Int
intervalMinutes (t1, t2) = dayMins + hourMins + minMins
    where dayMins = (date t2 - date t1) * 24 * 60
          hourMins = (hour t2 - hour t1) * 60
          minMins = minute t2 - minute t1

renderLog :: Log -> String
renderLog l = renderHead ++ concatMap renderEntry l

renderHead :: String
renderHead = "date, start, finish, total, description\n"

renderEntry :: LogEntry -> String
renderEntry (LogEntry comment ts _) = concatMap (renderInterval1 comment) ts

renderInterval1 :: Text -> (DateTime, DateTime) -> String
renderInterval1 comment (t1, t2) = printf "%02d %s,%02d:%02d,%02d:%02d,%d:%02d,%s\n" day mon h1 m1 h2 m2 hours minutes (unpack comment)
    where   mins = intervalMinutes (t1, t2)
            day = (date t1)
            mon = (unpack $ month t1)
            h1 = (hour t1)
            m1 = (minute t1)
            h2 = (hour t2)
            m2 = (minute t2)
            hours = (div mins 60)
            minutes = (mod mins 60)

renderTotalHours :: Int -> Text -> String
renderTotalHours mins id = printf "TOTAL,%.02f\nID,%s\n" hours id
    where   hours = (fromIntegral mins) / 60 :: Float

logParser :: Parser Log
logParser = ((some logEntryParser) <* endOfInput)

logEntryParser :: Parser LogEntry
logEntryParser = do
    comment <- takeTill ('\n' ==) <* endOfLine
    timesAndLines <- many timePairsWithLinesParser
    -- Try to parse a single incomplete time line (no matching end time)
    -- Only if we're not at a blank line (which would indicate end of entry)
    incompleteLineOpt <- optional $ do
        line <- takeTill ('\n' ==)
        if Data.Text.null line
            then fail "blank line"
            else endOfLine >> return line
    -- Consume trailing blank lines
    _ <- many (endOfLine)
    let (times, linesList) = Data.List.unzip timesAndLines
        allLines = Data.List.concat linesList ++ maybe [] (\l -> [fromStrict l]) incompleteLineOpt
    return $ LogEntry (fromStrict comment) times allLines

timePairsParser :: Parser (DateTime, DateTime)
timePairsParser = do
    time1 <- timeParser <|> timeParser2 <|> timeParser3 <|> timeParser4
    time2 <- timeParser <|> timeParser2 <|> timeParser3 <|> timeParser4
    return (time1, time2)

timePairsWithLinesParser :: Parser ((DateTime, DateTime), [Text])
timePairsWithLinesParser = do
    (line1, time1) <- DAT.match (timeParser <|> timeParser2 <|> timeParser3 <|> timeParser4)
    (line2, time2) <- DAT.match (timeParser <|> timeParser2 <|> timeParser3 <|> timeParser4)
    return ((time1, time2), [fromStrict $ stripEnd line1, fromStrict $ stripEnd line2])
    where stripEnd = Data.Text.takeWhile (/= '\n')

timezoneParser :: Parser Text
timezoneParser = fmap fromStrict $ ((DAT.string $ toStrict $ pack "AEST") <|> (DAT.string $ toStrict $ pack "AEDT") <|> (DAT.string $ toStrict $ pack "EST"))

ampmParser :: Parser Bool
ampmParser = fmap (toBool . unpack . fromStrict) $ ((DAT.string $ toStrict $ pack "AM") <|> (DAT.string $ toStrict $ pack "PM"))
    where
        toBool "AM" = True
        toBool "PM" = False

timeParser :: Parser DateTime
timeParser = do
    day <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    month <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    date <- parseDate <* char ' '
    hour <- (boundedDecimal 24) <* char ':'
    minute <- (boundedDecimal 60) <* char ':'
    second <- (boundedDecimal 60) <* char ' '
    timezone <- timezoneParser <* char ' '
    year <- (boundedDecimal 3000) <* endOfLine
    return $ DateTime (fromStrict day) (fromStrict month) (read date) hour minute second timezone year

timeParser2 :: Parser DateTime
timeParser2 = do
    day <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    date <- (some digit) <* char ' '
    month <- DAT.takeWhile (inClass "a-zA-Z") <* char ' ' <* char ' '
    hour <- (boundedDecimal 24) <* char ':'
    minute <- (boundedDecimal 60) <* char ':'
    second <- (boundedDecimal 60) <* char ' '
    timezone <- timezoneParser <* char ' '
    year <- (boundedDecimal 3000) <* endOfLine
    return $ DateTime (fromStrict day) (fromStrict month) (read date) hour minute second timezone year

timeParser3 :: Parser DateTime
timeParser3 = do
    day <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    month <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    date <- parseDate <* char ' '
    hour <- (boundedDecimal 13) <* char ':'
    minute <- (boundedDecimal 60) <* char ':'
    second <- (boundedDecimal 60) <* char ' '
    ampm <- ampmParser <* char ' '
    timezone <- timezoneParser <* char ' '
    year <- (boundedDecimal 3000) <* endOfLine
    return $ DateTime (fromStrict day) (fromStrict month) (read date) (convertHours ampm hour) minute second timezone year

timeParser4 :: Parser DateTime
timeParser4 = do
    day <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    date <- parseDate <* space
    month <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    year <- (boundedDecimal 3000) <* char ' '
    hour <- (boundedDecimal 24) <* char ':'
    minute <- (boundedDecimal 60) <* char ':'
    second <- (boundedDecimal 60) <* char ' '
    timezone <- timezoneParser <* endOfLine
    return $ DateTime (fromStrict day) (fromStrict month) (read date) hour minute second timezone year

parseDate = (char ' ' *> DAT.count 1 digit) <|> (DAT.count 2 digit)

boundedDecimal :: Int-> Parser Int
boundedDecimal max = do
    i <- decimal
    if (i :: Int) >= max
        then fail "decimal too big"
        else return i

convertHours :: Bool -> Int -> Int
convertHours True hours = hours
convertHours False hours
    | hours == 12 = hours
    | otherwise = hours + 12

base32chars :: [Int] -> Text
base32chars ints = pack $ map (chars !!) ints
    where chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

randomInts :: StdGen -> IO [Int]
randomInts gen = do
    let ints = randomRs (0,31) gen
    return ints
