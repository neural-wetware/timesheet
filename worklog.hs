import Data.Attoparsec.Text.Lazy
import qualified Data.Attoparsec.Text.Lazy as DAT
import Control.Applicative
import System.Environment
import Data.Text.Lazy hiding (map, concatMap, foldl', unlines)
import Data.List.Stream
import Prelude hiding ((++), map, concatMap, unlines)
import qualified Data.Text.Lazy.IO
import Text.Printf


template :: String -> String -> String -> String
template inv_no company table = unlines [
	"\\documentclass{article}",
	"\\usepackage{longtable}",
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
	"\\hline",
	table,
	"\\end{longtable}",
	"\\end{center}",
	"\\end{document}" ]

companies "formbay" = "Formbay Pty Ltd";
companies "lirrf" = "Lizard Island Reef Research Foundation";
companies "techsupply" = "Techsupply Pty Ltd";
companies x = error "invavlid company: " ++ x;

main = do
	[logFileName, invoiceNo, company] <- getArgs 
	logFile <- Data.Text.Lazy.IO.readFile $ logFileName
	case renderTemplate logFile of
		(Left err) -> putStr $ "Error: " ++ err ++ "\n" -- TODO go to stderr
		(Right log) -> putStr $ template invoiceNo company $ table log
	where table log = renderLog log ++ (renderTotal $ logMinutes log)

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

logMinutes :: Log -> Int
logMinutes logs = foldl' (+) 0 $ map entryMinutes logs

entryMinutes :: LogEntry -> Int
entryMinutes (LogEntry _ dts) = foldl' (+) 0 $ map intervalMinutes dts

intervalMinutes :: (DateTime, DateTime) -> Int
intervalMinutes (t1, t2) = ((hour t2 - hour t1) * 60) + (minute t2 - minute t1) -- TODO round up/down based on seconds. make sure to fix displayed minutes too!

renderLog :: Log -> String
renderLog = concatMap renderEntry

renderEntry :: LogEntry -> String
renderEntry (LogEntry comment times) = concatMap (renderInterval comment) times

renderInterval :: Text -> (DateTime, DateTime) -> String -- TODO check that dates match!!! -- TODO alter minutes based on rounding.
renderInterval comment (t1, t2) = printf "%02d %s &%02d:%02d - %02d:%02d &%d:%02d &%s \\\\\n" (date t1) (unpack $ month t1) (hour t1) (minute t1) (hour t2) (minute t2) (div mins 60) (mod mins 60) (unpack comment)
	where mins = intervalMinutes (t1, t2)

renderTotal :: Int -> String
renderTotal mins = printf "\\hline\nTOTAL & @\\$50/hour &%d:%02d &\\$%.02f (excl. GST) \\\\\n" (div mins 60) (mod mins 60) (((fromIntegral mins) / 60.0) * 50 :: Float)

logParser :: Parser Log
logParser = ((some logEntryParser) <* endOfInput)

logEntryParser :: Parser LogEntry
logEntryParser = do
	comment <- takeTill ('\n' ==) <* endOfLine -- TODO accept multiple line comments?
	times <- some timePairsParser
	return $ LogEntry (fromStrict comment) times

timePairsParser :: Parser (DateTime, DateTime)
timePairsParser = do
	time1 <- timeParser
	time2 <- timeParser
	return (time1, time2)

timeParser :: Parser DateTime
timeParser = do
	day <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
	month <- DAT.takeWhile (inClass "a-zA-Z") <* char ' '
	date <- parseDate <* char ' '
	hour <- DAT.count 2 digit <* char ':'
	minute <- DAT.count 2 digit <* char ':'
	second <- DAT.count 2 digit <* char ' '
	timezone <- DAT.string (toStrict $ pack "EST") <* char ' '
	year <-  DAT.count 4 digit <* some endOfLine
	return $ DateTime (fromStrict day) (fromStrict month) (read date) (read hour) (read minute) (read second) (fromStrict timezone) (read year)
	where
		parseDate = (char ' ' *> DAT.count 1 digit) <|> (DAT.count 2 digit)
