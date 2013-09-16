module Main where

import System.Environment(getArgs)

import FileIO
import Simplex

runAssignment :: PivotDict -> OutputFile
runAssignment pd =
    case findInOutVariables pd of
        Nothing          -> Unbounded
        Just (iV, oV, c) -> Bounded iV oV c

stepOne :: FilePath -> IO PivotingStepResult
stepOne file = do
    pd <- readDictFile file
    return $ pivotStep pd

run :: FilePath -> IO PivotingResult
run file = do
    pd <- readDictFile file
    return $ runPivoting pd

createAssignment1Answer :: FilePath -> IO ()
createAssignment1Answer path = do
    let outputPath = path ++ ".output"
    putStrLn $ "processing " ++ path ++ "..."
    pivotDict <- readDictFile path
    let output = runAssignment pivotDict
    writeOutputFile outputPath output 
    putStrLn $ "output written to " ++ outputPath

createAssignment2Answer :: FilePath -> IO ()
createAssignment2Answer file = do
    let outputPath = file ++ ".result"
    putStrLn $ "processing " ++ file ++ "..."
    res <- run file
    writeResultFile outputPath res
    putStrLn $ "output written to " ++ outputPath

main :: IO()
main = do
    args <- getArgs
    let path = args!!0
    createAssignment1Answer path