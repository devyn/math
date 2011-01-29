module Devyn.Factoring.NFactor
       (NFactor(fromNFactor, factors), toNFactor) where

{-
 - NFactor allows memoization of factors for a number within a calculation.
 - Devyn.Factoring will find the list of factors for a number many times
 - during a calculation, and that is very inefficient. A solution is to
 - attach the list of factors to the number while passing it down the chain.
 - This way, the list of factors is found only once.
 -}

data NFactor a = NFactor { fromNFactor :: a, factors :: [a] }
               deriving (Show, Eq, Ord)

toNFactor :: (Integral a) => a -> NFactor a

toNFactor n = NFactor n (findFactors n)

findFactors n
  | n <= 0    = []
  | even n    = 1 : 2 : f [3    .. n]
  | otherwise =     1 : f [3, 5 .. n]
  where     f = filter  ((== 0) . (mod n))