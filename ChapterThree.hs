module ChapterThree where

import ChapterOne
import ChapterTwo
import Data.Complex

marbleSystem :: [[Complex Double]] -> [Complex Double] -> Int -> [Complex Double]
marbleSystem m s i = vectMatAct (foldr (matMult) (identityMatComp (length (m !! 0))) (take i (repeat m))) s

-- 3.2.2, page 88
-- 3.3.2, page 97