module Geometry.Shapefile.Conduit
where

import Data.Conduit.Decoder
import Geometry.Shapefile.ReadShp

shapefileConduit = do
    hdr <- conduitDecoder getShpHeader
    conduitDecoder getShpRec
