module Main where

import System.Environment(getArgs)

import FileIO
import Simplex

runAssignment :: PivotDict -> OutputFile
runAssignment pd =
    case findInOutVariables pd of
        Nothing          -> Unbounded
        Just (iV, oV, c) -> Bounded iV oV c


main :: IO()
main = do
    args <- getArgs
    let path = args!!0
    let outputPath = path ++ ".output"
    putStrLn $ "processing " ++ path ++ "..."
    pivotDict <- readDictFile path
    let output = runAssignment pivotDict
    writeOutputFile outputPath output 
    putStrLn $ "output written to " ++ outputPath