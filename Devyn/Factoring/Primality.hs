{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, IncoherentInstances #-}

module Devyn.Factoring.Primality where

import Devyn.Factoring.NFactor

class Primality a where
  isPrime :: a -> Bool

instance Integral a => Primality (NFactor a) where
  isPrime = (== 2) . length . factors

instance Integral a => Primality a where
  -- |  Yes, this is  quite a long  string of conditions. And  yes, it
  -- does work. If you're skeptical, grab a list of 1000 known primes,
  -- and run the following:
  -- 
  -- >  'take' 1000 ('filter' 'isPrime' [1..]) == list
  -- 
  -- And then you should get 'True'. If not, something's really wrong,
  -- and you should let me know about that.
  -- 
  -- It's really quite fast now.
  isPrime n
    | n <= 1     = False
    | n == 2     = True
    | n == 3     = True
    | even n     = False -- not technically necessary, but might be faster
    |  nd6 (n+1)
    && nd6 (n-1) = False
    | otherwise  = length fas == 0
    where d m    = (== 0) . (`mod` m)
          nd6    = not . (d 6)
          sn     = (ceiling . sqrt . fromIntegral) n -- honestly, this could be better
          fal    = [5,11..sn] ++ [7,13..sn]
          fas    = filter ((== 0) . (mod n)) fal