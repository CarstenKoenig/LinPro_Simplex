module Simplex 
    ( PivotDict (..)
    , VarIndex
    , OutputFile (..)
    , findInOutVariables
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


data OutputFile = 
      Bounded   { inId :: Int, outId :: Int, newObjective :: Double }
    | Unbounded
    deriving (Show)    

findInOutVariables :: PivotDict -> Maybe (VarId, VarId, Double)
findInOutVariables pd = do
    inIndex       <- findInVariableIndex pd
    outIndex      <- findOutVariableIndex pd inIndex
    let c         = findNewObjectiveValue pd (inIndex, outIndex)
    return (getNonBasicId pd inIndex, getBasicId pd outIndex, objectiveValue pd + c)

objectiveValue :: PivotDict -> Double
objectiveValue = head . objectiveCoeffs

objectiveFunCoeffs :: PivotDict -> [Double]
objectiveFunCoeffs = tail . objectiveCoeffs

getA :: PivotDict -> (Int, Int) -> Double
getA pd (r,c) = (aMatrix pd) !! r !! c

getB :: PivotDict -> Int -> Double
getB pd r = bList pd !! r

getCoeff :: PivotDict -> Int -> Double
getCoeff pd index = objectiveFunCoeffs pd !! index

getBasicId :: PivotDict -> VarIndex -> VarId
getBasicId pd index = basicIds pd !! index

getNonBasicId :: PivotDict -> VarIndex -> VarId
getNonBasicId pd index = nonBasicIds pd !! index

isFeasible :: PivotDict -> Bool
isFeasible pd = all (>= 0) $ bList pd

findInVariableIndex :: PivotDict -> Maybe VarIndex
findInVariableIndex pd =
  if null candidates then Nothing else Just $ minimumBy comp candidates
  where candidates = [ i | i <- [0..nrNonBasic pd - 1], c i > 0]
        comp i j   = compare (getNonBasicId pd i) (getNonBasicId pd j)
        c i        = getCoeff pd i

findOutVariableIndex :: PivotDict -> VarIndex -> Maybe VarIndex
findOutVariableIndex pd inIndex = 
  if inIndex < 0 || null candidates then Nothing else Just $ minimumBy comp candidates
  where candidates = [ i | i <- [0..nrBasic pd - 1], a i < 0 && factor i > 0]
        comp i j = 
          case compare (factor i) (factor j) of
            EQ        -> compare (getBasicId pd i) (getBasicId pd j)
            x         -> x
        factor i   = b i / (- a i)
        a i        = getA pd (i, inIndex)
        b i        = getB pd i

findNewObjectiveValue :: PivotDict -> (VarIndex, VarIndex) -> Double
findNewObjectiveValue pd (inIndex, outIndex) = c outIndex
  where factor i   = b i / (- a i)
        c i        = getCoeff pd inIndex * factor i
        a i        = getA pd (i, inIndex)
        b i        = getB pd i