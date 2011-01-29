import System.Environment
import System.IO
import System.Exit
import System.CPUTime
import System.Locale
import Data.String.Utils
import Data.Time
import Text.Printf
import Data.GraphViz
import Devyn.Factoring
import Devyn.Factoring.Visualization
import Control.Parallel
import Numeric (showFFloat)
import Data.Tree

usage = ["- usage: bench <n> <log-dir>"]

main = getArgs >>= processArgs

processArgs (n : ldir : []) =
  doFT (toNFactor (read n)) ldir >>= either fail log
  where log info = do date <- getDateStamp
                      let le = join "," [n, showFFloat Nothing (time info) ""
                                        ,(show . length . flatten . tree) info, date]
                      appendFile (join "/" [ldir, "record.csv"]) (le++"\n")

processArgs _ = do mapM (hPutStrLn stderr) usage
                   exitWith (ExitFailure 2)

data Info = Info { time :: Double, out :: FilePath, tree :: Tree (NFactor Integer) }

doFT n odir = do
  start <- getCPUTime
  end   <- ft `pseq` getCPUTime
  cor   <- co
  either (return.Left)
    (res $ (fromIntegral (end - start))
           / (10^12)) cor
  where res d o = return $ Right
                         $ Info { time = d
                                , out  = o 
                                , tree = ft }
        co = runGraphvizCommand Dot gr Png filePath
        gr = ftToDot ft
        ft = factorTree n
        filePath = join "/" [odir, shows (fromNFactor n) ".png"]

getDateStamp = getCurrentTime
               >>= return . formatTime defaultTimeLocale
                              "%Y%m%d %Hh%M"