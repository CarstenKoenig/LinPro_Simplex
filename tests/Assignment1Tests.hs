module Assignment1Tests 
    ( testWithAssignmentTestsForProblem1
    , testWithAssignmentTestsForProblem2
    ) where


import Test.HUnit

import Control.Monad (forM)

import FileIO
import Simplex

near :: Double -> Double -> Bool
a `near` b = abs (b-a) < 0.0001

checkProblem1 :: (FilePath, (PivotDict, OutputFile)) -> Assertion
checkProblem1 (file, (pd, Unbounded)) = do
    putStrLn $ "checking unbounded case " ++ file
    case findInOutVariables pd of
        Nothing -> return ()
        Just (iV, oV, c) -> do
            assertFailure $ "not UNBOUNDED              - FAILED (" ++ show iV ++ ", " ++ show oV ++ ", " ++ show c ++ ")"
checkProblem1 (file, (pd, (Bounded iId oId cv))) =
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
            assertFailure $ "objective value            - FAILED (expected " ++ show cv ++ " but was " ++ show c ++ " switched " ++ show iV ++ "/" ++ show oV ++ ")"


checkProblem2 :: (FilePath, (PivotDict, ResultFile)) -> Assertion
checkProblem2 (file, (pd, UnboundedResult)) = do
    putStrLn $ "checking unbounded case " ++ file
    case runPivoting pd of
        ProblemUnbounded -> return ()
        Optimized _ _ -> do
            assertFailure $ "should be UNBOUNDED              - FAILED"
checkProblem2 (file, (pd, (BoundedResult val s))) = do
    putStrLn $ "checking bounded case " ++ file
    case runPivoting pd of
        ProblemUnbounded -> 
            assertFailure $ "should be BOUNDED                - FAILED"
        Optimized pd os -> do
            if os == s then return ()
            else 
                assertFailure $ "STEPS do not match         - FAILED (expected " ++ show s ++ " but was " ++ show os ++ ")"
            if objectiveValue pd `near` val then return ()
            else 
                assertFailure $ "objective value            - FAILED (expected " ++ show val ++ " but was " ++ show (objectiveValue pd) ++ ")"


loadAll :: (FilePath -> IO a) -> FilePath -> IO [ (FilePath, (PivotDict, a))]
loadAll rf fPath = do
    files <- findDictFiles fPath
    forM files readPair
    where 
        readPair file = do
            pd <- readDictFile file
            op <- rf file
            return (file, (pd, op))

checkAllProblem1 :: [(FilePath, (PivotDict, OutputFile))] -> Assertion
checkAllProblem1 = mapM_ checkProblem1

checkAllProblem2 :: [(FilePath, (PivotDict, ResultFile))] -> Assertion
checkAllProblem2 = mapM_ checkProblem2

testWithAssignmentTestsForProblem1 :: FilePath -> Assertion
testWithAssignmentTestsForProblem1 fPath = do
    files <- loadAll readOutputForDictFile fPath
    checkAllProblem1 files

testWithAssignmentTestsForProblem2 :: FilePath -> Assertion
testWithAssignmentTestsForProblem2 fPath = do
    files <- loadAll readResultForDictFile fPath
    checkAllProblem2 files