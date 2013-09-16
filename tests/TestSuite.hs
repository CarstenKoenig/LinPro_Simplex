module Main (
    main
 ) where

import Test.Framework
import Test.Framework.Providers.HUnit

import Assignment1Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "assignment 1 test cases"
    [
     testCase 
        "test cases of assignment1 from file system" $
        testWithAssignmentTestsForProblem1 ".\\part1TestCases\\unitTests"
    ], 
    testGroup "assignment 2 test cases"
    [
     testCase 
        "test cases of assignment2 from file system" $
        testWithAssignmentTestsForProblem2 ".\\part2TestCases\\unitTests"
    ]
  ]