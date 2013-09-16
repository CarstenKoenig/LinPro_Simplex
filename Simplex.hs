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
    , basicIds        :: [VarId]      -- ^ these are the indicies of the basic variables
    , nonBasicIds     :: [VarId]      -- ^ these are the indicies of the non-basic variables
    , bList           :: [Double]     -- ^ these comes from the original problem-data
    , aMatrix         :: [[Double]]   -- ^ these are the constraint coefficients for the non-basic variables
    , objectiveCoeffs :: [Double]     -- ^ these are the coefficents for the objective function (c_0 and c_1 ... c_n)
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
objectiveValue = head . objectiveCoeffs

objectiveFunCoeffs :: PivotDict -> [Double]
objectiveFunCoeffs = tail . objectiveCoeffs

getA :: PivotDict -> (Int, Int) -> Double
getA pd (r,c) = 
  if r >= nrBasic pd then error $ "row-index to high: " ++ show r ++ " [limit: " ++ show (nrBasic pd) ++ "]"
  else if c >= nrNonBasic pd then error $ "col-index to high: " ++ show c ++ " [limit: " ++ show (nrNonBasic pd) ++ "]"
  else (aMatrix pd) !! r !! c

getB :: PivotDict -> Int -> Double
getB pd r = bList pd !! r

getCoeff :: PivotDict -> Int -> Double
getCoeff pd index = objectiveFunCoeffs pd !! index

getBasicId :: PivotDict -> VarIndex -> VarId
getBasicId pd index = basicIds pd !! index

getNonBasicId :: PivotDict -> VarIndex -> VarId
getNonBasicId pd index = nonBasicIds pd !! index

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
pivoting pd (indIn, indOut) = PivotDict (nrBasic pd) (nrNonBasic pd) basicIds' nonBasicIds' bs' as' cs'
  where basicIds'    = replaceAt (basicIds pd) indOut (getNonBasicId pd indIn)
        nonBasicIds' = replaceAt (nonBasicIds pd) indIn (getBasicId pd indOut)
        bs'          = [ mapB i    | i <- [0..nrBasic pd -1]]
        as'          = [ [mapA i j | j <- [0..nrNonBasic pd -1]] | i <- [0..nrBasic pd -1] ]
        cs'          = cv' : [ mapC i | i <- [0..nrNonBasic pd -1]]
        mapB i
          | i == indOut = bout
          | otherwise   = b i + a (i, indIn) * bout
        mapA i j
          | i == indOut && j == indIn = -factor
          | i == indOut && j /= indIn = factor * a (indOut, j)
          | i /= indOut && j == indIn = -factor * a (i, indIn)
          | otherwise                 = a (i,j) + a (i, indIn) * a (indOut, j) * factor
        mapC i
          | i == indIn = -factor * c indIn
          | otherwise  = c i + c indIn * a (indOut, i) * factor
        bout     = b indOut * factor
        cv'      = objectiveValue pd + bout * c indIn
        factor   = (-1) / a (indOut, indIn)
        a (i, j) = getA pd (i, j)
        b i      = getB pd i
        c i      = getCoeff pd i

replaceAt :: [a] -> Int -> a -> [a]
replaceAt [] _ _ = []
replaceAt (h:tl) i v
  | i == 0    = v:tl
  | otherwise = h:replaceAt tl (i-1) v