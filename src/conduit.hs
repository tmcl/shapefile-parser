module Main
where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Shapefile.Conduit
import System.Environment
import Text.Show.Pretty
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC

main :: IO ()
main = getArgs >>= mapM_ (runResourceT . testConduit)

testConduit filePath = 
    CB.sourceFile filePath =$= 
    dbfConduit $$ 
    CC.mapM_ (liftIO . putStrLn . ppShow)
