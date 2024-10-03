{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module ChapterTwo where

import ChapterOne
import Data.Complex

-- performs the addition of two complex vector spaces
vectAdd :: [Complex Double] -> [Complex Double] -> [Complex Double]
vectAdd [] _ = []
vectAdd (x : xs) (y : ys) = addComplex x y : vectAdd xs ys

-- returns the inverse of a complex vector space
vectInv :: [Complex Double] -> [Complex Double]
vectInv [] = []
vectInv (x : xs) = ((-realPart x) :+ (-imagPart x)) : vectInv xs

-- performs the scalar multiplication of a complex vector space
vectScMult :: Complex Double -> [Complex Double] -> [Complex Double]
vectScMult _ [] = []
vectScMult z (x : xs) = multComplex z x : vectScMult z xs

-- visual representation of coordinates in c^m*n
{-
c^(mxn) matrix:
\|c(0,0)      c(0,1)        ...      c(0,n-1)  |
\|c(1,0)      c(1,1)        ...      c(1,n-1)  |
\|...         ...           ...      ...       |
\|c(m-1,0)    c(c-1,1)      ...      c(m-1,n-1)|
-}

-- performs the addition of two elements in c^m*n
matAdd :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
matAdd [] _ = []
matAdd (x : xs) (y : ys) = (zipWith (addComplex) x y) : (matAdd xs ys)

-- tools to return the inverse, transpose, conjugate and adjoint of two elements in c^m*n
matInv, matTrans, matConj, matAdj :: [[Complex Double]] -> [[Complex Double]]
matInv xs = map (vectInv) xs

-- performs the scalar multiplication of an element in c^m*n
matScMult :: Complex Double -> [[Complex Double]] -> [[Complex Double]]
matScMult z [] = []
matScMult z (x : xs) = map (multComplex z) x : matScMult z xs

matTrans [] = []
matTrans xs = [[x !! n | x <- xs] | n <- [0 .. length (head xs) - 1]]

matConj [] = []
matConj (x : xs) = [conjComplex y | y <- x] : matConj xs

matAdj xs = matTrans (matConj xs)

-- performs matrix multiplication for two elements of c^m*n (of the appropriate size)
matMult :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
matMult [] _ = []
matMult x y = [oneElement 0 b | b <- [0 .. b1]] : matMult (tail (x)) y
  where
    oneElement a b = foldr (addComplex) (0) (zipWith (multComplex) (x !! a) (matTrans (y) !! b))
    b1 = length (matTrans (y)) - 1

-- A function that accepts a vector and a matrix and outputs the vector resulting from the “action.”
vectMatAct :: [[Complex Double]] -> [Complex Double] -> [Complex Double]
vectMatAct x y = [element a y | a <- x]
  where
    element a y = foldr (addComplex) (0) (zipWith (multComplex) a y)

-- A function that accepts two complex vectors and calculates their inner product
vectInnProd :: [Complex Double] -> [Complex Double] -> Complex Double
vectInnProd x y = foldr (addComplex) (0) (zipWith (multComplex) (conj x) y)
  where
    conj as = [conjComplex a | a <- as]

-- A function that calculates the norm of a given complex vector
vectNorm :: [Complex Double] -> Double
vectNorm xs = sqrt (foldr (+) (0) [(realPart (x) ** 2) + (imagPart (x) ** 2) | x <- xs])

-- A function that calculates the distance of two given complex vectors
vectDist :: [Complex Double] -> [Complex Double] -> Double
vectDist x y = vectNorm (vectAdd x (vectScMult (-1) y))

-- Functions that accept a square matrix and tells if they are hermitian/unitary
isMatHermitian, isMatUnitary :: [[Complex Double]] -> Bool
isMatHermitian x = x == matAdj x

-- Creates a real identity matrix of a given order
identityMat :: Int -> [[Int]]
identityMat x = [[if a == i then 1 else 0 | a <- [0 .. (x - 1)]] | i <- [0 .. (x - 1)]]

-- Creates a complex identity matrix of a given order
identityMatComp :: Int -> [[Complex Double]]
identityMatComp x = [[if a == i then 1 else 0 | a <- [0 .. (x - 1)]] | i <- [0 .. (x - 1)]]

isMatUnitary x = (matMult x (matAdj x) == matMult (matAdj x) x) && (matMult x (matAdj x) == identityMatComp (length (x !! 0)))

testUnit1 = [[0.5 :+ 0.5,(-0.5) :+ 0.5],[0.5 :+ 0.5,0.5 :+ (-0.5)]]
test2 = [[1.0 :+ 3.0, 4.0 :+ 0.0],[2.0 :+ (-1.0), 1.0 :+ 1.0],[(-2.0) :+ 4.0, 0.0 :+ 1.0]]


-- A function that accepts two matrices and constructs their tensor product
tensorProd :: [[Complex Double]] -> [[Complex Double]] -> [[Complex Double]]
tensorProd xss yss = [concat [(vectScMult (xss !! xrow !! xelem) (yss !! yrow)) | xelem <- [0..((length (xss !! 0)) - 1)]] | xrow <- [0..((length xss) - 1)], yrow <- [0..((length yss) - 1)]]

-- Testing tensor product function logic with real integers
{-
realVSM :: Int -> [Int] -> [Int]
realVSM _ [] = []
realVSM z (x : xs) = (z * x) : realVSM z xs

rt1 = [[1,2],[3,4]] :: [[Int]]
rt2 = [[2,4],[1,3],[5,2]] :: [[Int]]

mtp :: [[Int]] -> [[Int]] -> [[Int]]
mtp xss yss = [concat [(realVSM (xss !! xrow !! xelem) (yss !! yrow)) | xelem <- [0..((length (xss !! 0)) - 1)]] | xrow <- [0..((length xss) - 1)], yrow <- [0..((length yss) - 1)]]
-}