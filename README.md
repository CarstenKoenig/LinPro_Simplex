# Assignment 1

this implements a very simple solution to the first two assignments given in the coursera course [linear and integer programming](https://class.coursera.org/linearprogramming-001/assignment/index) using Haskell

## usage

### tests
included are a few tests that runs the assignments testcase-files
    cabal test
for example should run the code if you do not forget to enable the tests
    cabal install --dependencies-only --enable-tests
	cabal configure --enable-tests

### generating output

for assignment 1 you can just call the executable with a path to a pivot-dictionary in the format descriped [here](https://class.coursera.org/linearprogramming-001/assignment/view?assignment_id=5). This wil create a file with the same name extended with a .output suffix.

for assignment 2 you have to load main.hs into ghci and call the function
    createAssignment2Answer path
with a path to a valid pivot-dictionary file creating a output-file with the same name extended with a .result suffix.
