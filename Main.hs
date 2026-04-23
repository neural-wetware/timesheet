import System.Environment
import System.Random
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Directory (copyFile)
import Control.Monad (when)
import qualified Data.Text.Lazy.IO
import qualified Data.Map.Strict as Map
import qualified Data.List

import Timesheet

main :: IO ()
main = do
    [logFileName] <- getArgs
    logFile <- Data.Text.Lazy.IO.readFile $ logFileName
    today <- utctDay <$> getCurrentTime
    let currentWeekStart = getCurrentWeekStart today

    case renderTemplate logFile of
        (Left err) -> putStr $ "Error: " ++ err ++ "\n"
        (Right allEntries) -> do
            let (currentWeekEntries, completedEntries) = partitionByWeek currentWeekStart allEntries
                weekGroups = Map.toList $ groupByWeek completedEntries

            when (not $ Data.List.null weekGroups) $ do
                gens <- getStdGen >>= randomInts
                let gensList = Data.List.take (Data.List.length weekGroups) $ chunksList 8 gens
                mapM_ (\((weekStart, weekLog), weekGen) -> writeWeekFiles weekGen logFile weekStart weekLog)
                      (Data.List.zip weekGroups gensList)
                putStrLn $ "Generated " ++ show (Data.List.length weekGroups) ++ " week(s)"

                -- Create backup and rewrite input file with current week entries only
                let backupFileName = logFileName ++ ".bak"
                copyFile logFileName backupFileName
                putStrLn $ "Created backup: " ++ backupFileName

                Data.Text.Lazy.IO.writeFile logFileName (renderWorkLog currentWeekEntries)
                putStrLn $ "Updated " ++ logFileName ++ " (kept " ++ show (Data.List.length currentWeekEntries) ++ " current week entries)"

            when (Data.List.null weekGroups) $
                putStrLn "No completed weeks to process"
