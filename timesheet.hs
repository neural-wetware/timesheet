import Data.Attoparsec.Text.Lazy
import qualified Data.Attoparsec.Text.Lazy as DAT
import Control.Applicative
import System.Environment
import Data.Text.Lazy hiding (map, concatMap, foldl', length)
import Data.List
import Prelude hiding ((++), map, concatMap, unlines, length)
import qualified Data.Text.Lazy.IO
import Text.Printf

template :: Log -> String
template log = Data.List.unlines [
    renderLog log,
    renderTotalHours mins ]
    where mins = logMinutes log

logMinutes :: Log -> Int
logMinutes logs = foldl' (+) 0 $ map entryMinutes logs

entryMinutes :: LogEntry -> Int
entryMinutes (LogEntry _ dts) = foldl' (+) 0 $ map intervalMinutes dts

main = do
    [logFileName] <- getArgs 
    logFile <- Data.Text.Lazy.IO.readFile $ logFileName
    case renderTemplate logFile of
        (Left err) -> putStr $ "Error: " ++ err ++ "\n" -- TODO go to stderr
        (Right log) -> putStr $ template log

renderTemplate :: Text -> Either String Log
renderTemplate logFile = eitherResult $ parse logParser logFile

type Log = [LogEntry]

data LogEntry = LogEntry {
    comment :: Text,
    times :: [(DateTime, DateTime)]
} deriving Show

data DateTime = DateTime {
    day :: Text,
    month :: Text,
    date :: Int,
    hour :: Int,
    minute :: Int,
    second :: Int,
    timezone :: Text,
    year :: Int
} deriving Show

intervalMinutes :: (DateTime, DateTime) -> Int
intervalMinutes (t1, t2) = dayMins + hourMins + minMins -- TODO round up/down based on seconds. make sure to fix displayed minutes too!
    where dayMins = (date t2 - date t1) * 24 * 60
          hourMins = (hour t2 - hour t1) * 60
          minMins = minute t2 - minute t1

renderLog :: Log -> String
renderLog l = renderHead ++ concatMap renderEntry l

renderHead :: String
renderHead = "date, start, finish, total, description\n"

renderEntry :: LogEntry -> String
renderEntry (LogEntry comment ts) = concatMap (renderInterval1 comment) ts

renderInterval1 :: Text -> (DateTime, DateTime) -> String -- TODO check that dates match!!! -- TODO alter minutes based on rounding.
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

renderTotalHours :: Int -> String
renderTotalHours mins = printf ",,,%d:%02d,TOTAL\n" hours minutes
    where   hours = div mins 60
            minutes = mod mins 60

logParser :: Parser Log
logParser = ((some logEntryParser) <* endOfInput)

logEntryParser :: Parser LogEntry
logEntryParser = do
    comment <- takeTill ('\n' ==) <* endOfLine
    times <- some timePairsParser
    return $ LogEntry (fromStrict comment) times

timePairsParser :: Parser (DateTime, DateTime)
timePairsParser = do
    time1 <- timeParser <|> timeParser2
    time2 <- timeParser <|> timeParser2
    return (time1, time2)

timezoneParser :: Parser Text
timezoneParser = fmap fromStrict $ ((DAT.string $ toStrict $ pack "AEST") <|> (DAT.string $ toStrict $ pack "AEDT") <|> (DAT.string $ toStrict $ pack "EST"))

timeParser :: Parser DateTime
timeParser = do
    day <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    month <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    date <- parseDate <* char ' '
    hour <- DAT.count 2 digit <* char ':'
    minute <- DAT.count 2 digit <* char ':'
    second <- DAT.count 2 digit <* char ' '
    timezone <- timezoneParser <* char ' '
    year <-  DAT.count 4 digit <* some endOfLine
    return $ DateTime (fromStrict day) (fromStrict month) (read date) (read hour) (read minute) (read second) timezone (read year)
    where
        parseDate = (char ' ' *> DAT.count 1 digit) <|> (DAT.count 2 digit)

timeParser2 :: Parser DateTime
timeParser2 = do
    day <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
    date <- (some digit) <* char ' '
    month <- DAT.takeWhile (inClass "a-zA-Z") <* char ' ' <* char ' '
    hour <- DAT.count 2 digit <* char ':'
    minute <- DAT.count 2 digit <* char ':'
    second <- DAT.count 2 digit <* char ' '
    timezone <- timezoneParser <* char ' '
    year <-  DAT.count 4 digit <* some endOfLine
    return $ DateTime (fromStrict day) (fromStrict month) (read date) (read hour) (read minute) (read second) timezone (read year)
