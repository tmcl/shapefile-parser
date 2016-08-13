module Geometry.Shapefile.Conduit
where

import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.Combinators as CC
import Data.Conduit.Decoder
import Data.Dbase.Conduit
import Geometry.Shapefile.ReadShp
import Geometry.Shapefile
import Control.Monad.Trans.Resource
import System.FilePath

shapefileConduit :: Conduit ByteString (ResourceT IO) ShpRec
shapefileConduit = do
    CC.dropE 100
    conduitDecoder getShpRec

shpDbfConduit :: FilePath -> Source (ResourceT IO) (ShpRec, DbfRow)
shpDbfConduit filePath = 
   zipSources 
      (sourceFile (filePath `replaceExtension` "shp") =$= shapefileConduit) 
      (sourceFile (filePath `replaceExtension` "dbf") =$= dbfConduit)
