-- output a tree of prime factors  for a given natural number. has the
-- ability to time itself.  currently, without any parallelization, it
-- runs on my 2.26 GHz Intel Core  2 Duo at 3s for the number 27520516
-- (which is 5246^2)

import System.Environment
import System.IO
import System.Exit
import System.CPUTime
import Text.Printf
import Data.GraphViz
import Devyn.Factoring.Visualization

usage = ["- usage: factor_tree [-b] <n> [<output.png>]            "
        ,"  if invoked with -b, the operation time will be printed"
        ,"  if given a file name, will draw an image of the tree  "
        ,"                                         in PNG format  "]

main = getArgs >>= processArgs

processArgs (        "-b" : as) = time (processArgs as)
processArgs (n : filePath : []) = outputImage (read n) filePath
processArgs (           n : []) = outputFlat  (read n)

processArgs _ = do mapM (hPutStrLn stderr) usage
                   exitWith (ExitFailure 2)

outputImage n filePath =
  runGraphvizCommand Dot gr Png filePath
  >>= either fail (const $ return ())
  where gr = factorDot n

time a = do
  putStrLn "%% start"
  start <- getCPUTime
  v     <- a
  end   <- getCPUTime
  putStrLn "%% end"
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "%%%% time: %f seconds\n" (diff :: Double)
  return v