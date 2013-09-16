module FileIO 
    ( readDictFile
    , readOutputForDictFile
    , readResultForDictFile
    , writeOutputFile
    , findDictFiles
    , writeResultFile
    ) where


import Control.Monad (liftM)
import Data.List (isSuffixOf, isPrefixOf)
import System.Directory (getDirectoryContents)
import System.FilePath((</>), addExtension)

import Simplex


readDictFile :: FilePath -> IO PivotDict
readDictFile =  (liftM readPivotDict) . fileContent


readOutputForDictFile :: FilePath -> IO OutputFile
readOutputForDictFile = (liftM $ readOutputFile) . fileContent . getOutputFile


readResultForDictFile :: FilePath -> IO ResultFile
readResultForDictFile = (liftM $ readResultFile) . fileContent . getOutputFile


writeOutputFile :: FilePath -> OutputFile -> IO ()
writeOutputFile path output =  writeFile path $ writeOutput output


writeResultFile :: FilePath -> PivotingResult -> IO ()
writeResultFile path output =  writeFile path $ writeResult output


fileContent :: FilePath -> IO String
fileContent = readFile


readPivotDict :: FilePath -> PivotDict
readPivotDict input = PivotDict m n bis nis bs as ocs
    where   m    = readWord 0 $ line 0
            n    = readWord 1 $ line 0
            bis  = readInts $ line 1
            nis  = readInts $ line 2
            bs   = readDbls $ line 3
            as   = map (readDbls . line) [4 .. 3+m]
            ocs  = readDbls $ line (m+4)
            readInts :: String -> [Int]
            readInts = map read . words
            readDbls :: String -> [Double]
            readDbls = map read . words
            readWord i s =  read $ (words s) !! i
            line i = lns !! i
            lns = lines input 


readOutputFile :: String -> OutputFile
readOutputFile input = 
    if line 0 == "UNBOUNDED" then
        Unbounded
    else 
        Bounded iI oI c
    where   iI   = read $ line 0
            oI   = read $ line 1
            c    = read $ line 2
            line i = lns !! i
            lns = lines input 

readResultFile :: String -> ResultFile
readResultFile input = 
    if line 0 == "UNBOUNDED" then
        UnboundedResult
    else 
        BoundedResult val s
    where   val   = read $ line 0
            s     = read $ line 1
            line i = lns !! i
            lns = lines input 

writeOutput :: OutputFile -> String
writeOutput Unbounded                = "UNBOUNDED"
writeOutput (Bounded iId oId obj) = show iId ++ "\n" ++ show oId ++ "\n" ++ show obj ++ "\n"

writeResult :: PivotingResult -> String
writeResult ProblemUnbounded = "UNBOUNDED"
writeResult (Optimized pd s) = show (objectiveValue pd) ++ "\n" ++ show s ++ "\n"

findDictFiles :: FilePath -> IO [FilePath]
findDictFiles inPath = do
    contents <- getDirectoryContents inPath
    return $ map (inPath </>) . filter find $ contents
    where find fn = (not $ isSuffixOf ".output" fn) && isPrefixOf "dict" fn


getOutputFile :: FilePath -> FilePath
getOutputFile fp = addExtension fp  "output"