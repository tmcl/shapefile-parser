module Main
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Geometry.Shapefile.Conduit
import System.Environment
import Text.Show.Pretty

main :: IO ()
main = do
    [fp1, fp2] <- getArgs
    runResourceT $ shpDbfSource Nothing fp1 >> shpDbfSource Nothing fp2 $$ CC.mapM_ (liftIO . putStrLn . ppShow)

testConduit :: FilePath -> ResourceT IO ()
testConduit filePath =
  shpDbfSource Nothing filePath $$
    CC.mapM_ (liftIO . putStrLn . ppShow)
