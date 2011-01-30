module Devyn.Factoring.Visualization
       (
         outputFlat,
         outputFT,
         outputDot,
         factorDot,
         ftToDot,
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

outputFlat = outputFT . factorTree . toNFactor

outputFT :: (Integral a)
         => Tree (NFactor a)
         -> IO ()

outputFT tree = putStrLn $ drawTree $ re `fmap` tree
  where  re n | isPrime n = shows (fromNFactor n) " (prime)"
              | otherwise = shows (fromNFactor n) " (composite)"

outputDot :: (Integral a)
          => a
          -> IO ()

outputDot = putStrLn . printDotGraph . factorDot

factorDot :: (Integral a)
          => a
          -> DotGraph Node

factorDot n = ftToDot $ factorTree (toNFactor n)

ftToDot :: (Integral a)
        => Tree (NFactor a)
        -> DotGraph Node

ftToDot t = graphToDot gvParams (graphFT t)

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

tNodes = flatten . numberTree

tEdges :: Tree a
       -> [(Int, Int, ())]

tEdges n = c (numberTree n)
  where c (Node (i,l) cs) = cs >>= (\ ch@(Node (ci,cl) ccs)
                                       -> (i,ci,()):(c ch))

numberTree :: Tree a
           -> Tree (Int, a)

numberTree n = evalState (mapM (\ a -> do s <- get
                                          put (s + 1)
                                          return (s,a)) n) 0