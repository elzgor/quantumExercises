module ChapterThree where

import ChapterOne
import ChapterTwo
import Data.Complex

{-
Write a program that performs our little marble experiment. The program should allow the user to enter a Boolean matrix that describes the
ways that marbles move. Make sure that the matrix follows our requirement. The user
should also be permitted to enter a starting state of how many marbles are on each
vertex. Then the user enters how many time clicks she wants to proceed. The computer should then calculate and output the state of the system after those time clicks.

Translation:
Enter a square boolean matrix, B
Enter a vector representing the starting state of a system, s0
Enter the number of "time clicks", x
s1 = B * s0
s2 = B * s1
...
output = B * (x-1)
-}
marbleSystem :: [[Complex Double]] -> [Complex Double] -> Int -> [Complex Double]
marbleSystem m s i = vectMatAct (foldr (matMult) (identityMatComp (length (m !! 0))) (take i (repeat m))) s

-- 3.2.2, page 88
{-
What would happen if there were more than two slits?
Write a program that asks a user to design a multislit experiment. The user notes
the number of slits and the number of targets to measure the bullets. Then the user
enters probabilities of the bullets’ moving from each slit to each target. An appropriate
matrix is set up and then the matrix is multiplied by itself. Have the program print the
appropriate resulting matrix and vect
-}
-- 3.3.1&2, page 97
{-
Modify your program from Programming Drill 3.2.1 so
that you allow the entries to be complex numbers as opposed to fractions.

Modify your program from Programming Drill 3.2.2 so
that you allow transitions from the many slits to the many measuring devices to be
complex numbers. Your program should identify where there are interference phenomen
-}
