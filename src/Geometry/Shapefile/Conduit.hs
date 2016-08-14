module Geometry.Shapefile.Conduit (shapefileConduit, shpDbfConduit)
where

import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.Decoder
import Data.Dbase.Conduit
import Geometry.Shapefile.ReadShp
import Geometry.Shapefile
import Control.Monad.Trans.Resource
import System.FilePath
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class

shapefileConduit :: (MonadIO m, MonadThrow m) => Conduit ByteString m (ShpHeader, ShpRec)
shapefileConduit = do
    hdr <- replicateM 100 CC.headE
    conduitDecoder getShpRec =$= CC.map ((,) $ parseHeader hdr)
    where 
       parseHeader = runGet getShpHeader . BL.pack . catMaybes

shpDbfConduit :: FilePath -> Source (ResourceT IO) (ShpHeader, ShpRec, DbfRow)
shpDbfConduit filePath = zipSources shp dbf =$= CC.map (\((a, b), c) -> (a, b, c))
   where 
      shp = sourceWithExtension filePath "shp" =$= shapefileConduit
      dbf = sourceWithExtension filePath "dbf" =$= dbfConduit

sourceWithExtension :: FilePath -> String -> Source (ResourceT IO) ByteString
sourceWithExtension filePath ext = CC.sourceFile (filePath `replaceExtension` ext)
