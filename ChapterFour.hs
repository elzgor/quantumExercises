module ChapterFour where

import ChapterOne
import ChapterThree
import ChapterTwo
import Data.Complex

-- The below function probOneKet describes a quantum system of a particle on a single line: a = how many points the particle can occupy, xs = the relative complex amplitudes of the ket state vector, b = the specific point at which the probability is calculated (i.e. [0..(a-1)]).

probOneKet :: Int -> [Complex Double] -> Int -> Double
probOneKet a xs b = (normComplex (xs !! b)) / normEq xs
  where
    normEq xs = foldr (+) (0) [(realPart (x) ** 2) + (imagPart (x) ** 2) | x <- xs]

-- probTwoKets: The user enters 2 kets, and we calculate the probability of transitioning from the first ket to the second after an observation has been made.

probTwoKets :: [Complex Double] -> [Complex Double] -> Complex Double
probTwoKets [] _ = 0
probTwoKets xs ys = (multComplex (head xs) (conjComplex (head ys))) `addComplex` (probTwoKets (tail xs) (tail ys))

normComplex :: Complex Double -> Double
normComplex (a :+ b) = (a ** 2 + b ** 2)

normKets :: [Complex Double] -> [Complex Double]
normKets x = [divComplex a m | a <- x]
  where
    m = ((sqrt (foldr (+) (0) [normComplex xs | xs <- x])) :+ 0)

vectAdj :: [Complex Double] -> [Complex Double]
vectAdj [] = []
vectAdj xs = (conjComplex (head (xs))) : vectAdj (tail (xs))

-- Mean and variance of the operator acting on the ket. IN PROGRESS
observable :: [[Complex Double]] -> [Complex Double] -> Complex Double
observable mat ket = probTwoKets (matTrans (matMult mat [ket]) !! 0) ket

-- enter observable and state vector, return list of eigenvalues of the observable?


-- input a number of time steps n
