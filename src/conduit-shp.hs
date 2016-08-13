module Main
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import Geometry.Shapefile.Conduit
import System.Environment
import Text.Show.Pretty

main :: IO ()
main = getArgs >>= mapM_ (runResourceT . testConduit)

testConduit :: FilePath -> ResourceT IO ()
testConduit filePath = 
    CB.sourceFile filePath =$= 
    shapefileConduit $$ 
    CC.mapM_ (liftIO . putStrLn . ppShow)
