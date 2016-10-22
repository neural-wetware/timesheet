import Data.Attoparsec.Text.Lazy
import qualified Data.Attoparsec.Text.Lazy as DAT
import Control.Applicative
import System.Environment
import Data.Text.Lazy hiding (map, concatMap, foldl', length)
import Data.List
import Prelude hiding ((++), map, concatMap, unlines, length)
import qualified Data.Text.Lazy.IO
import Text.Printf

rate = 70

template :: String -> String -> Log -> String
template inv_no company log = Data.List.unlines [
    "\\documentclass{article}",
    "\\usepackage{longtable}",
    "\\usepackage{multirow}",
    "\\title{TAX INVOICE}",
    "\\author{Daniel Lagos}",
    "",
    "\\begin{document}",
    "",
    "\\maketitle",
    "\\begin{tabular}{ll}",
    "Address: 161/3-9 Church Ave, Mascot & Invoice no: " ++ inv_no ++ " \\\\",
    "Mobile: 0415 205 542 & To: " ++ companies company ++ " \\\\",
    "ABN: 58 228 211 683 & \\\\",
    "\\end{tabular}",
    "",
    "\\maketitle",
    "\\begin{center}",
    "\\begin{longtable}{l|l|l|p{2in}}",
    "\\multicolumn{1}{c}{Date} & \\multicolumn{1}{c}{Times} & \\multicolumn{1}{c}{Hours} & \\multicolumn{1}{c}{Description} \\\\",
    renderLog log,
    renderTotalHours mins,
    "\\end{longtable}",
    "",
    "\\begin{longtable}{r|r|l}",
    renderTotal mins,
    renderGST mins,
    renderAmountPayable mins,
    "\\hline",
    "\\end{longtable}",
    "\\end{center}",
    "\\end{document}" ]
    where mins = logMinutes log

logMinutes :: Log -> Int
logMinutes logs = foldl' (+) 0 $ map entryMinutes logs

entryMinutes :: LogEntry -> Int
entryMinutes (LogEntry _ dts) = foldl' (+) 0 $ map intervalMinutes dts

companies "formbay" = "Formbay Pty Ltd";
companies "lirrf" = "Lizard Island Reef Research Foundation";
companies "techsupply" = "Techsupply Pty Ltd";
companies x = error "invavlid company: " ++ x;

main = do
    [logFileName, invoiceNo, company] <- getArgs 
    logFile <- Data.Text.Lazy.IO.readFile $ logFileName
    case renderTemplate logFile of
        (Left err) -> putStr $ "Error: " ++ err ++ "\n" -- TODO go to stderr
        (Right log) -> putStr $ template invoiceNo company log

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
renderLog = concatMap renderEntry

renderEntry :: LogEntry -> String
renderEntry (LogEntry comment (t:ts)) = "\\hline \n" ++ renderInterval1 comment (length (t:ts)) t ++ concatMap renderInterval2 ts

renderInterval1 :: Text -> Int -> (DateTime, DateTime) -> String -- TODO check that dates match!!! -- TODO alter minutes based on rounding.
renderInterval1 comment rows (t1, t2) = printf "\\multirow{%d}{*}{%02d %s} &%02d:%02d - %02d:%02d &%d:%02d &\\multirow{%d}{*}{%s} \\\\\n" rows (date t1) (unpack $ month t1) (hour t1) (minute t1) (hour t2) (minute t2) (div mins 60) (mod mins 60) rows (unpack comment)
    where    mins = intervalMinutes (t1, t2)

renderInterval2 :: (DateTime, DateTime) -> String -- TODO check that dates match!!! -- TODO alter minutes based on rounding.
renderInterval2 (t1, t2) = printf "\t&%02d:%02d - %02d:%02d &%d:%02d & \\\\\n" (hour t1) (minute t1) (hour t2) (minute t2) (div mins 60) (mod mins 60)
    where    mins = intervalMinutes (t1, t2)

renderTotalHours :: Int -> String -- TODO use "rate" instead of hard code $70
renderTotalHours mins = printf "\\hline \n & & %d:%02d & @\\$70/hour \\\\\n" hours minutes
    where   hours = div mins 60
            minutes = mod mins 60

renderTotal :: Int -> String -- TODO use "rate" instead of hard code $70
renderTotal mins = printf "\\hline \nTOTAL & \\$%.02f & excl GST \\\\\n" (dollars mins)

renderGST :: Int -> String
renderGST mins = printf "\\hline \nGST & \\$%.02f & \\\\\n" (gst mins)

renderAmountPayable :: Int -> String
renderAmountPayable mins = printf "\\hline \nAMMOUNT PAYABLE & \\$%.02f & incl GST \\\\\n" (amount mins)

gst mins = (dollars mins) * 0.10

amount mins = (dollars mins) * 1.10

dollars mins = ((fromIntegral mins) / 60.0) * rate :: Float

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
