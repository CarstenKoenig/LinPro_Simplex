module Simplex 
    ( PivotDict (..)
    , PivotingStepResult (..)
    , PivotingResult (..)
    , VarIndex
    , findInOutVariables
    , pivotStep
    , runPivoting
    , objectiveValue
    ) where

import Data.List (minimumBy)
import qualified Data.Vector as V
import SimplexVector

-- | used for pivoting - index for variables (maps Index -> Variable ID)
type VarIndex = Int
type VarId    = Int

{- |
    representation of a pivot-dictionary for the simplex algorithm in the form:

    x_b = b - A x_i
    z   = c_0 + c^t x_i
    
    where x_b is a m-vector of basic variables { x_b1, x_b2, .., x_bm }
          x_i is a n-vector of non-basic variables {x_i1, ... x_in }
          b is a m-vector with the constraint-values of the original problem
          A is a mxn-matrix indicating the coefficients of the constraints
          z is the objective
          c_0 is a constant and c (a n-vector) are the coefficients of the objective function
-}
data PivotDict = PivotDict
    { nrBasic         :: Int          -- ^ this is the number of basic variables (initial the slack variables)
    , nrNonBasic      :: Int          -- ^ this is the number on non-basic variables
    , basicIds        :: Vector VarId -- ^ these are the indicies of the basic variables
    , nonBasicIds     :: Vector VarId -- ^ these are the indicies of the non-basic variables
    , matrix          :: DoubleMatrix -- ^ represents the complete problem - rows are for the basic-variables (last one is the objective), first row is b (last one is the objective-value)
    } deriving (Show)

{- |
  result of a single pivoting step
  Finished indicates that the simplex-algorithm cannot find a better solution (indeed it should be optimal)
  Intermediate indicates that there are possible more steps to improve the result
  IsUnbounded indicates that there is no optimal solution because the objective function can be made arbitrarily large
-}
data PivotingStepResult =
    Finished PivotDict
  | Intermediate PivotDict  
  | IsUnbounded
  deriving (Show)

{- |
  result of a simplex run
  Optimized indicates that the simplex-algorithm cannot find a better solution (indeed it should be optimal)
  ProblemUnbounded indicates that there is no optimal solution because the objective function can be made arbitrarily large
-}
data PivotingResult =
    Optimized { result :: PivotDict, steps :: Int }
  | ProblemUnbounded
  deriving (Show)

-- | helper function for the assignment 
-- tries to calculate the input variable-id (the variable that will get moved into the basic-variables)
-- and the output variable-id (the variable that will get moved from the basic-variables into the non-basic variables)
findInOutVariables :: PivotDict -> Maybe (VarId, VarId, Double)
findInOutVariables pd = do
    inIndex       <- findInVariableIndex pd
    outIndex      <- findOutVariableIndex pd inIndex
    let pd'       = pivoting pd (inIndex, outIndex)
    let c         = objectiveValue pd'
    return (getNonBasicId pd inIndex, getBasicId pd outIndex, c)

-- | intermediate function that performs a single pivoting-operation on a Pivot-Dictionary
-- if it cannot find a in-Variable the problem should be solved
-- if it cannot find a output-variable to the found input-variable the problem is unbounded
-- in every other case it will perform the pivoting step using the pivoting function
pivotStep :: PivotDict -> PivotingStepResult
pivotStep pd =
  case findInVariableIndex pd of
    Nothing      -> Finished pd
    Just inIndex -> case findOutVariableIndex pd inIndex of
                      Nothing       -> IsUnbounded 
                      Just outIndex -> Intermediate $ pivoting pd (inIndex, outIndex)

-- | runs pivotStep as long as it can be improved (Intermediate result)
runPivoting :: PivotDict -> PivotingResult
runPivoting = runner 0
  where 
    runner i pd =
      case pivotStep pd of
        Finished pd'     -> Optimized pd' i
        IsUnbounded      -> ProblemUnbounded
        Intermediate pd' -> runner (i+1) pd'

-- | extracts the objective-value of a given Pivot-Dictionary
objectiveValue :: PivotDict -> Double
objectiveValue = V.head . objectiveCoeffs

objectiveCoeffs :: PivotDict -> DoubleVector
objectiveCoeffs = V.last . matrix

objectiveFunCoeffs :: PivotDict -> DoubleVector
objectiveFunCoeffs = V.tail . objectiveCoeffs

getRow :: PivotDict -> Int -> DoubleVector
getRow pd r = matrix pd .! r

getA :: PivotDict -> (Int, Int) -> Double
getA pd (r,c) = 
  if r > nrBasic pd then error $ "row-index to high: " ++ show r ++ " [limit: " ++ show (nrBasic pd) ++ "]"
  else if c >= nrNonBasic pd then error $ "col-index to high: " ++ show c ++ " [limit: " ++ show (nrNonBasic pd) ++ "]"
  else (matrix pd) .!! (r, c+1)

getB :: PivotDict -> Int -> Double
getB pd r = (matrix pd) .!! (r, 0)

getCoeff :: PivotDict -> Int -> Double
getCoeff pd index = objectiveFunCoeffs pd .! index

getBasicId :: PivotDict -> VarIndex -> VarId
getBasicId pd index = basicIds pd .! index

getNonBasicId :: PivotDict -> VarIndex -> VarId
getNonBasicId pd index = nonBasicIds pd .! index

findInVariableIndex :: PivotDict -> Maybe VarIndex
findInVariableIndex pd =
  if null candidates then Nothing else Just $ minimumBy comp candidates
  where candidates = [ i | i <- [0..nrNonBasic pd - 1], c i > 0]
        comp i j   = compare (getNonBasicId pd i) (getNonBasicId pd j)
        c i        = getCoeff pd i

findOutVariableIndex :: PivotDict -> VarIndex -> Maybe VarIndex
findOutVariableIndex pd inIndex = 
  if inIndex < 0 || null candidates then Nothing else Just $ minimumBy comp candidates
  where candidates = [ i | i <- [0..nrBasic pd - 1], a i < 0]
        comp i j = 
          case compare (boundedf i) (boundedf j) of
            EQ        -> compare (getBasicId pd i) (getBasicId pd j)
            x         -> x
        factor i   = b i / (- a i)
        boundedf i = let f = factor i in
                     if f <= 0 then 0 else f
        a i        = getA pd (i, inIndex)
        b i        = getB pd i

pivoting :: PivotDict -> (VarIndex, VarIndex) -> PivotDict
pivoting pd (indIn, indOut) = PivotDict (nrBasic pd) (nrNonBasic pd) basicIds' nonBasicIds' matrix'
  where basicIds'    = basicIds pd ./ (indOut, getNonBasicId pd indIn)
        nonBasicIds' = nonBasicIds pd ./ (indIn, getBasicId pd indOut)
        matrix'      = vcreate [ mapA i    | i <- [0..nrBasic pd ]]
        base         = vcreate (bout : [if j == indIn then -factor else factor * a (indOut, j) | j <- [0..nrNonBasic pd -1]])
        mapA i
          | i == indOut = base
          | otherwise   = row i .+. a (i, indIn) .* base
        bout     = b indOut * factor
        factor   = (-1) / a (indOut, indIn)
        a (i, j) = getA pd (i, j)
        b i      = getB pd i
        row i    = getRow pd i ./ (indIn+1, 0)