module Assignment1Tests 
    ( testWithAssignmentTests
    ) where


import Test.HUnit

import Control.Monad (forM)

import FileIO
import Simplex

near :: Double -> Double -> Bool
a `near` b = abs (b-a) < 0.0001

check :: (FilePath, (PivotDict, OutputFile)) -> Assertion
check (file, (pd, Unbounded)) = do
    putStrLn $ "checking unbounded case " ++ file
    case findInOutVariables pd of
        Nothing -> return ()
        Just (iV, oV, c) -> do
            assertFailure $ "not UNBOUNDED              - FAILED (" ++ show iV ++ ", " ++ show oV ++ ", " ++ show c ++ ")"
check (file, (pd, (Bounded iId oId cv))) =
    let (iV, oV, c) = maybe (-1, -1, -1) id $ findInOutVariables pd
    in do
        putStrLn $ "checking bounded case " ++ file
        if iV == iId then return ()
        else 
            assertFailure $ "IN Variables do not match  - FAILED (expected " ++ show iId ++ " but was " ++ show iV ++ ")"
        if oV == oId then return ()
        else 
            assertFailure $ "OUT Variables do not match - FAILED (expected " ++ show oId ++ " but was " ++ show oV ++ ")"
        if c `near` cv then return ()
        else 
            assertFailure $ "objective value            - FAILED (expected " ++ show cv ++ " but was " ++ show c ++ ")"


loadAll :: FilePath -> IO [ (FilePath, (PivotDict, OutputFile))]
loadAll fPath = do
    files <- findDictFiles fPath
    forM files readPair
    where 
        readPair :: FilePath -> IO (FilePath, (PivotDict, OutputFile))
        readPair file = do
            pd <- readDictFile file
            op <- readOutputForDictFile file
            return (file, (pd, op))

checkAll :: [(FilePath, (PivotDict, OutputFile))] -> Assertion
checkAll = mapM_ check

-- A recommended way of creating HUnit tests. Such tests are easy to integrate
-- with test-framework (see MainTestSuite.hs)
testWithAssignmentTests :: FilePath -> Assertion
testWithAssignmentTests fPath = do
    files <- loadAll fPath
    checkAll files