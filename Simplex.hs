module Simplex 
    ( PivotDict (..)
    , PivotingStepResult (..)
    , PivotingResult (..)
    , VarIndex
    , OutputFile (..)
    , ResultFile (..)
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


data PivotingStepResult =
    Finished PivotDict
  | Intermediate PivotDict  
  | IsUnbounded
  deriving (Show)

data PivotingResult =
    Optimized { result :: PivotDict, steps :: Int }
  | ProblemUnbounded
  deriving (Show)

data OutputFile = 
      Bounded   { inId :: Int, outId :: Int, newObjective :: Double }
    | Unbounded
    deriving (Show)    

data ResultFile = 
      BoundedResult   { value :: Double, nrSteps :: Int }
    | UnboundedResult
    deriving (Show)    

findInOutVariables :: PivotDict -> Maybe (VarId, VarId, Double)
findInOutVariables pd = do
    inIndex       <- findInVariableIndex pd
    outIndex      <- findOutVariableIndex pd inIndex
    let pd'       = pivoting pd (inIndex, outIndex)
    let c         = objectiveValue pd'
    return (getNonBasicId pd inIndex, getBasicId pd outIndex, c)

pivotStep :: PivotDict -> PivotingStepResult
pivotStep pd =
  case findInVariableIndex pd of
    Nothing      -> Finished pd
    Just inIndex -> case findOutVariableIndex pd inIndex of
                      Nothing       -> IsUnbounded 
                      Just outIndex -> Intermediate $ pivoting pd (inIndex, outIndex)

runPivoting :: PivotDict -> PivotingResult
runPivoting = runner 0
  where 
    runner i pd =
      case pivotStep pd of
        Finished pd'     -> Optimized pd' i
        IsUnbounded      -> ProblemUnbounded
        Intermediate pd' -> runner (i+1) pd'

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
          | i /= indOut && j /= indIn = a (i,j) + a (i, indIn) * a (indOut, j) * factor
        mapC i
          | i == indIn = -factor * c indIn
          | otherwise  = c i + c indIn * a (indOut, i) * factor
        a (i, j) = getA pd (i, j)
        aswitch  = a (indOut, indIn)
        b i      = getB pd i
        bout     = b indOut * factor
        c i      = getCoeff pd i
        cv       = objectiveValue pd
        cv'      = cv + bout * c indIn
        factor   = (-1) / aswitch
        replaceAt ls ind v = [ if i == ind then v else ls!!i | i <- [0..length ls -1]]