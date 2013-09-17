module SimplexVector 
    ( Vector, IntVector, DoubleVector
    , Matrix, IntMatrix, DoubleMatrix
    , vcreate
    , vget, vset
    , vadd, vscal
    , (.!), (./), (.+.), (.*)
    , mcreate
    , mget, mset
    , (.!!), (.//)
    ) where

import qualified Data.Vector as V
import Data.Vector ((!), (//))

type Vector a     = V.Vector a
type IntVector    = V.Vector Int
type DoubleVector = V.Vector Double

type Matrix a     = V.Vector (V.Vector a)
type IntMatrix    = Matrix Int
type DoubleMatrix = Matrix Double

vcreate :: [a] -> V.Vector a
vcreate as = V.fromList as

mcreate :: [[a]] -> Matrix a
mcreate = V.fromList . map V.fromList

vget :: V.Vector a -> Int -> a
vget v i = v ! i

vset :: V.Vector a -> Int -> a -> V.Vector a
vset v i a = v // [(i, a)]

mget :: Matrix a -> (Int, Int) -> a
mget v (r,c) = (v ! r) ! c

mset :: Matrix a -> (Int, Int) -> a -> Matrix a
mset m (r,c) a = m // [(r, v')]
    where v' = vset v c a
          v  = vget m r

vadd :: Num a => V.Vector a -> V.Vector a -> V.Vector a
a `vadd` b = V.zipWith (+) a b

vscal :: Num a => a -> V.Vector a -> V.Vector a
s `vscal` a = V.map (s*) a

(.!) :: V.Vector a -> Int -> a
a .! i = vget a i

(./) :: V.Vector a -> (Int, a) -> V.Vector a
v ./ (i, a) = vset v i a

(.!!) :: Matrix a -> (Int, Int) -> a
m .!! (r,c) = mget m (r,c)

(.//) :: Matrix a -> ((Int, Int), a) -> Matrix a
m .// ((r,c), a) = mset m (r,c) a

infixr 0 .+.
(.+.) :: Num a => V.Vector a -> V.Vector a -> V.Vector a
a .+. b = a `vadd` b

infixr 1 .*
(.*) :: Num a => a -> V.Vector a -> V.Vector a
s .* v = s `vscal` v