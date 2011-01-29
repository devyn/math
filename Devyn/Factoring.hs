module Devyn.Factoring
       (
         gpf,
         gpfUpto,
         isPrime,
         gcf,
         factorTree,
         NFactor,
         toNFactor,
         fromNFactor,
         factors,
       ) where

import Data.List
import Data.Tree
import Devyn.Factoring.NFactor

-- | Calculate the greatest prime factor of a number.
gpf :: (Integral a)
    => NFactor a
    -> a

gpf n = maximum $ filter (isPrime . toNFactor) (factors n)

-- | Calculate the greatest prime factor of a number up to a limit.  A
-- possible use for this would be  to calculate the prime factor up to
-- 7, to factorize a number as small numbers.
gpfUpto :: (Integral a)
        => a         -- ^ The limit to the greatest prime factor.
        -> NFactor a -- ^ The number to calculate the GPF for.
        -> a         -- ^ The greatest prime factor.

gpfUpto max n = maximum $ filter (\ x -> x <= max
                                      && isPrime (toNFactor x)) (factors n)

-- | Simple, inefficient primality test.
isPrime :: (Integral a)
        => NFactor a
        -> Bool

isPrime n
  | nv <= 1       = False
  | nv == 2       = True
  | nv == 3       = True
  | mod nv 2 == 0 = False
  | mod nv 3 == 0 = False
  | otherwise     = length (factors n) <= 2
  where        nv = fromNFactor n

-- | Find the greatest common factor of two numbers.
gcf :: (Integral a)
    => NFactor a
    -> NFactor a
    -> a

gcf x y = maximum $ factors x `intersect` factors y

-- no lcm, use Prelude.lcm instead.

-- | Outputs a factor tree for a given number.
-- It's currently quite slow due to the functions it depends on.
factorTree :: (Integral a)
           => NFactor a
           -> Tree (NFactor a)

factorTree n
  | isPrime n = Node n []
  | otherwise = let f = gpf n
                    in Node n [Node (toNFactor f) []
                              ,factorTree (toNFactor
                                           ((fromNFactor n) `quot` f))]