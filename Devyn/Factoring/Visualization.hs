module Devyn.Factoring.Visualization
       (
         outputFlat,
         outputDot,
         factorDot,
         graphFT
       ) where

import Prelude hiding (mapM)

import Devyn.Factoring

import Data.Graph.Inductive
import Data.Tree
import Data.GraphViz
import Data.Traversable (mapM)
import Control.Monad.State hiding (mapM)
import Numeric (showFFloat)

outputFlat :: (Integral a)
           => a
           -> IO ()

outputFlat n = putStrLn $ drawTree $ re `fmap` factorTree (toNFactor n)
  where re n | isPrime n = shows (fromNFactor n) " (prime)"
             | otherwise = shows (fromNFactor n) " (composite)"

outputDot :: (Integral a)
          => a
          -> IO ()

outputDot = putStrLn . printDotGraph . factorDot

factorDot :: (Integral a)
          => a
          -> DotGraph Node

factorDot n = graphToDot gvParams (graphFT $ factorTree (toNFactor n))

gvParams :: (Integral a)
         => GraphvizParams (NFactor a) el () (NFactor a)

gvParams = nonClusteredParams { globalAttributes = [
                                   GraphAttrs [
                                      (BgColor . X11Color) Transparent
                                   ],
                                   NodeAttrs [
                                     Shape Circle,
                                     Style [SItem Filled []]
                                   ]
                                ]
                              , fmtNode = fn
                              , fmtEdge = fe }
  where fn (n,l) = [(Label . StrLabel) (show $ fromNFactor l)
                   ,if isPrime l
                       then (FillColor . X11Color) Yellow
                       else (FillColor . X11Color) White]
        fe (f,t,l) = []

graphFT :: (Integral a)
        => Tree (NFactor a)
        -> Gr (NFactor a) ()

graphFT n = mkGraph (tNodes n) (tEdges n)

tNodes :: Tree a
       -> [(Int, a)]

tNodes = reverse . snd . foldl fn (0,[]) . flatten
  where fn (a,ls) l = (a+1,(a,l):ls)

tEdges :: Tree a
       -> [(Int, Int, ())]

tEdges n = c n'
  where n' = evalState (mapM (\ a -> do s <- get
                                        put (s + 1)
                                        return (s,a)) n) 0
        c (Node (i,l) cs) = cs >>= (\ ch@(Node (ci,cl) ccs) -> (i,ci,()):(c ch))