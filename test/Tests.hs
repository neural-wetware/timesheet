import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import qualified Data.Map.Strict as Map
import System.Directory
import System.FilePath
import Data.List (sort)

import Timesheet

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Timesheet Tests"
    [ parsingTests
    , weekGroupingTests
    , formatPreservationTests
    , currentWeekFilteringTests
    , incompleteEntryTests
    , csvGenerationTests
    ]

parsingTests :: TestTree
parsingTests = testGroup "Parsing Tests"
    [ testCase "Parse simple entry" $ do
        let input = TL.pack "office\nTue Aug 12 03:25:29 PM AEST 2025\nTue Aug 12 06:12:49 PM AEST 2025\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right [entry] -> do
                TL.unpack (comment entry) @?= "office"
                length (times entry) @?= 1
            Right entries -> assertFailure $ "Expected 1 entry, got " ++ show (length entries)

    , testCase "Parse multiple entries" $ do
        let input = TL.pack $ unlines
                [ "office"
                , "Tue Aug 12 03:25:29 PM AEST 2025"
                , "Tue Aug 12 06:12:49 PM AEST 2025"
                , ""
                , "home"
                , "Tue 12 Aug 2025 19:59:26 AEST"
                , "Tue 12 Aug 2025 21:21:45 AEST"
                ]
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> length entries @?= 2

    , testCase "Parse entry with AM/PM times" $ do
        let input = TL.pack "office\nMon Mar 30 10:44:23 AM AEDT 2026\nMon Mar 30 02:03:48 PM AEDT 2026\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right [entry] -> do
                let [(t1, t2)] = times entry
                hour t1 @?= 10  -- 10 AM
                hour t2 @?= 14  -- 2 PM

    , testCase "Parse incomplete entry (start time only)" $ do
        let input = TL.pack "office\nThu Apr 24 09:00:00 AM AEST 2026\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right [entry] -> do
                TL.unpack (comment entry) @?= "office"
                length (times entry) @?= 0  -- No complete time pairs
                length (originalLines entry) @?= 1  -- But has the incomplete line

    , testCase "Parse entry with multiple time pairs" $ do
        let input = TL.pack $ unlines
                [ "office"
                , "Wed Aug 13 12:38:29 PM AEST 2025"
                , "Wed Aug 13 01:20:29 PM AEST 2025"
                , "Wed Aug 13 02:13:19 PM AEST 2025"
                , "Wed Aug 13 05:33:19 PM AEST 2025"
                ]
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right [entry] -> length (times entry) @?= 2
    ]

weekGroupingTests :: TestTree
weekGroupingTests = testGroup "Week Grouping Tests"
    [ testCase "Group entries by week" $ do
        let input = TL.pack $ unlines
                [ "home"
                , "Sat Dec 27 12:16:31 PM AEDT 2025"
                , "Sat Dec 27 12:46:31 PM AEDT 2025"
                , ""
                , "office"
                , "Mon Jan  5 09:00:00 AM AEDT 2026"
                , "Mon Jan  5 05:00:00 PM AEDT 2026"
                ]
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let weekGroups = Map.toList $ groupByWeek entries
                length weekGroups @?= 2  -- Two different weeks

    , testCase "Entries in same week are grouped together" $ do
        let input = TL.pack $ unlines
                [ "office"
                , "Mon Mar 30 10:44:23 AM AEDT 2026"
                , "Mon Mar 30 02:03:48 PM AEDT 2026"
                , ""
                , "home"
                , "Thu Apr  2 05:59:56 PM AEDT 2026"
                , "Thu Apr  2 08:00:29 PM AEDT 2026"
                ]
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let weekGroups = Map.toList $ groupByWeek entries
                length weekGroups @?= 1  -- Same week (Mar 30 - Apr 5)
                case weekGroups of
                    [(_, groupedEntries)] -> length groupedEntries @?= 2
                    _ -> assertFailure "Expected exactly one week group"

    , testCase "Week starts on Monday" $ do
        -- Apr 2, 2026 is a Thursday, so its week starts on Mar 30 (Monday)
        let input = TL.pack "office\nThu Apr  2 05:59:56 PM AEDT 2026\nThu Apr  2 08:00:29 PM AEDT 2026\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right [entry] -> do
                let weekStart = getWeekStart $ fst $ head $ times entry
                    (y, m, d) = toGregorian weekStart
                (y, m, d) @?= (2026, 3, 30)  -- March 30, 2026 (Monday)
    ]

formatPreservationTests :: TestTree
formatPreservationTests = testGroup "Format Preservation Tests"
    [ testCase "Preserve AM/PM format in work.log" $ do
        let input = TL.pack "office\nMon Mar 30 10:44:23 AM AEDT 2026\nMon Mar 30 02:03:48 PM AEDT 2026\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let workLog = renderWorkLog entries
                TL.isInfixOf (TL.pack "10:44:23 AM") workLog @? "Should preserve AM format"
                TL.isInfixOf (TL.pack "02:03:48 PM") workLog @? "Should preserve PM format"

    , testCase "Preserve original time format exactly" $ do
        let originalLine = "Tue Aug 12 03:25:29 PM AEST 2025"
            input = TL.pack $ "office\n" ++ originalLine ++ "\nTue Aug 12 06:12:49 PM AEST 2025\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let workLog = renderWorkLog entries
                TL.isInfixOf (TL.pack originalLine) workLog @? "Should preserve exact format"

    , testCase "Preserve entry comments" $ do
        let input = TL.pack "home\nSat Dec 27 12:16:31 PM AEDT 2025\nSat Dec 27 12:46:31 PM AEDT 2025\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let workLog = renderWorkLog entries
                TL.isInfixOf (TL.pack "home") workLog @? "Should preserve comment"
    ]

currentWeekFilteringTests :: TestTree
currentWeekFilteringTests = testGroup "Current Week Filtering Tests"
    [ testCase "Filter current week entries" $ do
        let today = fromGregorian 2026 4 23  -- Apr 23, 2026 (Thursday)
            currentWeekStart = getCurrentWeekStart today
            input = TL.pack $ unlines
                [ "office"
                , "Mon Mar 30 10:44:23 AM AEDT 2026"
                , "Mon Mar 30 02:03:48 PM AEDT 2026"
                , ""
                , "office"
                , "Wed Apr 23 10:55:04 AM AEST 2026"
                , "Wed Apr 23 02:30:15 PM AEST 2026"
                ]
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let (currentWeek, completed) = partitionByWeek currentWeekStart entries
                length currentWeek @?= 1  -- Apr 23 entry
                length completed @?= 1    -- Mar 30 entry

    , testCase "Incomplete entries stay in current week" $ do
        let today = fromGregorian 2026 4 23
            currentWeekStart = getCurrentWeekStart today
            input = TL.pack "office\nThu Apr 24 09:00:00 AM AEST 2026\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let (currentWeek, completed) = partitionByWeek currentWeekStart entries
                length currentWeek @?= 1
                length completed @?= 0
    ]

incompleteEntryTests :: TestTree
incompleteEntryTests = testGroup "Incomplete Entry Tests"
    [ testCase "Handle entry with only start time" $ do
        let input = TL.pack "office\nThu Apr 24 09:00:00 AM AEST 2026\n"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right [entry] -> do
                length (times entry) @?= 0
                length (originalLines entry) @?= 1

    , testCase "Mix of complete and incomplete entries" $ do
        let input = TL.pack $ unlines
                [ "office"
                , "Wed Apr 23 10:55:04 AM AEST 2026"
                , "Wed Apr 23 02:30:15 PM AEST 2026"
                , ""
                , "office"
                , "Thu Apr 24 09:00:00 AM AEST 2026"
                ]
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                length entries @?= 2
                -- First entry should be complete
                length (times (entries !! 0)) @?= 1
                -- Second entry should be incomplete
                length (times (entries !! 1)) @?= 0
    ]

csvGenerationTests :: TestTree
csvGenerationTests = testGroup "CSV Generation Tests"
    [ testCase "Generate CSV with correct format" $ do
        let input = TL.pack "office\nTue Aug 12 03:25:29 PM AEST 2025\nTue Aug 12 06:12:49 PM AEST 2025\n"
            testId = TL.pack "TESTID12"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let csv = template entries testId
                -- Check for header
                assertBool "Should have header" $ "date, start, finish, total, description" `elem` lines csv
                -- Check for ID
                assertBool "Should have ID" $ "ID,TESTID12" `elem` lines csv
                -- Check for total
                assertBool "Should have TOTAL" $ any (isPrefixOf "TOTAL,") (lines csv)

    , testCase "Calculate total hours correctly" $ do
        let input = TL.pack "office\nTue Aug 12 03:25:29 PM AEST 2025\nTue Aug 12 06:12:49 PM AEST 2025\n"
            testId = TL.pack "TEST1234"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let totalMins = logMinutes entries
                -- 15:25 to 18:12 is 2 hours 47 minutes = 167 minutes
                totalMins @?= 167

    , testCase "Handle multiple entries in CSV" $ do
        let input = TL.pack $ unlines
                [ "office"
                , "Wed Aug 13 12:38:29 PM AEST 2025"
                , "Wed Aug 13 01:20:29 PM AEST 2025"
                , "Wed Aug 13 02:13:19 PM AEST 2025"
                , "Wed Aug 13 05:33:19 PM AEST 2025"
                ]
            testId = TL.pack "MULTI123"
        case renderTemplate input of
            Left err -> assertFailure $ "Parse failed: " ++ err
            Right entries -> do
                let csv = template entries testId
                    csvLines = filter (not . null) $ lines csv
                -- Should have 2 data lines (2 time pairs) + header + total + ID = 5 lines
                length csvLines @?= 5
    ]

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
