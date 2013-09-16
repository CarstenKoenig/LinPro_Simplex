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
        testWithAssignmentTests ".\\part1TestCases\\unitTests"
    ]
  ]