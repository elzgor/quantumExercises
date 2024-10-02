module ChapterOne where

import Data.Complex

-- tools to add, multiply, subtract and divide complex numbers
addComplex, multComplex, subComplex, divComplex :: Complex Double -> Complex Double -> Complex Double
addComplex a b = c
  where
    c = (realPart a + realPart b) :+ (imagPart a + imagPart b)
multComplex a b = rec :+ imc
  where
    rec = ((realPart a * realPart b) - (imagPart a * imagPart b))
    imc = ((realPart a * imagPart b) + (realPart b * imagPart a))
subComplex a b = c
  where
    c = (realPart a - realPart b) :+ (imagPart a - imagPart b)
divComplex (a1 :+ a2) (b1 :+ b2) = rec :+ imc
  where
    rec = ((a1 * b1) + (a2 * b2)) / (modComplex (b1 :+ b2) ** 2)
    imc = ((a2 * b1) - (a1 * b2)) / (modComplex (b1 :+ b2) ** 2)

-- outputs the modulus of a complex number
modComplex :: Complex Double -> Double
modComplex (a :+ b) = sqrt (a ** 2 + b ** 2)

-- outputs the conjugate of a complex number
conjComplex :: Complex Double -> Complex Double
conjComplex (a :+ b) = a :+ (-b)

-- converts a complex number from its Cartesian representation to its polar representation and vice versa
cartToPol, polToCart :: (Double, Double) -> (Double, Double)
cartToPol (a, b) = (rho, theta)
  where
    rho = modComplex (a :+ b)
    theta = atan (b / a)
polToCart (rho, theta) = (a, b)
  where
    a = rho * cos (theta)
    b = rho * sin (theta)

-- tools to multiply and divide complex numbers in their polar form
multPolForm, divPolForm :: (Double, Double) -> (Double, Double) -> (Double, Double)
multPolForm (rho1, theta1) (rho2, theta2) = (rho, theta)
  where
    rho = rho1 * rho2
    theta = theta1 + theta2
divPolForm (rho1, theta1) (rho2, theta2) = (rho, theta)
  where
    rho = rho1 / rho2
    theta = theta1 - theta2

{-
Programming Drill 1.3.2 If you like graphics, write a program that accepts a small
drawing around the origin of the complex plane and a complex number. The program
should change the drawing by multiplying every point of the diagram by a complex
number.
-}

{-
rootsOfUnity :: (Double, Double) -> Int -> Int -> [(Double, Double)]
rootsOfUnity (rho, theta) n m = [(a, b) | a <- (rho ** (1 / n)), b <- ((1 / n) * (theta + ( * 2 * pi))), k = [1 .. n]]
-}
