module Geometry.Shapefile.Conduit
where

import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Combinators as CC
import Data.Conduit.Decoder
import Geometry.Shapefile.ReadShp
import Geometry.Shapefile
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

shapefileConduit :: Conduit ByteString (ResourceT IO) ShpRec
shapefileConduit = do
    hdr <- toConsumer $ CC.takeE 100 =$= sinkNull
    conduitDecoder getShpRec
