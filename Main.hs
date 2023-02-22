import Data.Attoparsec.Text.Lazy
import qualified Data.Attoparsec.Text.Lazy as DAT
import Control.Applicative
import System.Random
import System.Environment
import Data.Text.Lazy hiding (map, concatMap, foldl', length)
import Data.List
import Prelude hiding ((++), map, concatMap, unlines, length)
import qualified Data.Text.Lazy.IO
import Text.Printf

template :: Log -> Text -> String
template log id = Data.List.unlines [
    renderLog log,
    renderTotalHours mins id ]
    where mins = logMinutes log

logMinutes :: Log -> Int
logMinutes logs = foldl' (+) 0 $ map entryMinutes logs

entryMinutes :: LogEntry -> Int
entryMinutes (LogEntry _ dts) = foldl' (+) 0 $ map intervalMinutes dts

main = do
    gen <- getStdGen
    ints <- randomInts gen
    [logFileName] <- getArgs
    logFile <- Data.Text.Lazy.IO.readFile $ logFileName
    case renderTemplate logFile of
        (Left err) -> putStr $ "Error: " ++ err ++ "\n" -- TODO go to stderr
        (Right log) -> putStr $ template log (Data.Text.Lazy.take 8 $ base32chars ints)

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

renderTotalHours :: Int -> Text -> String
renderTotalHours mins id = printf "TOTAL,%.02f\nID,%s\n" hours id
    where   hours = (fromIntegral mins) / 60 :: Float

logParser :: Parser Log
logParser = ((some logEntryParser) <* endOfInput)

logEntryParser :: Parser LogEntry
logEntryParser = do
    comment <- takeTill ('\n' ==) <* endOfLine
    times <- some timePairsParser
    return $ LogEntry (fromStrict comment) times

timePairsParser :: Parser (DateTime, DateTime)
timePairsParser = do
    time1 <- timeParser <|> timeParser2 <|> timeParser3 <|> timeParser4
    time2 <- timeParser <|> timeParser2 <|> timeParser3 <|> timeParser4
    return (time1, time2)

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
    year <- (boundedDecimal 3000) <* some endOfLine
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
    year <- (boundedDecimal 3000) <* some endOfLine
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
    year <- (boundedDecimal 3000) <* some endOfLine
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
    timezone <- timezoneParser <* some endOfLine
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
