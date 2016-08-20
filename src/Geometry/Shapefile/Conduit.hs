module Geometry.Shapefile.Conduit (shapefileConduit, shpDbfConduit, shpDbfSource, Shape, module Geometry.Shapefile, shapesByName, sourceFromStart, shapeFieldByColumnNameRule, matchTextDbfField, shapesFromDbfShpSource, matchNumericDbfField)
where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Binary.Get
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit.Decoder
import           Data.Conduit.Internal (zipSources)
import           Data.Dbase.Conduit
import           Data.Maybe
import Data.Text (Text)
import           Geometry.Shapefile
import           System.FilePath
import System.IO

type Shape = (ShpHeader, ShpRec, DbfRow)

sourceFromStart :: (MonadIO m) => Handle -> ConduitM i ByteString m ()
sourceFromStart handle = CB.sourceHandleRange handle (Just 0) Nothing

shapefileConduit :: (MonadIO m, MonadThrow m) => Maybe RecBBox -> Conduit ByteString m (ShpHeader, ShpRec)
shapefileConduit boundingBox = do
    hdr <- replicateM 100 CC.headE
    let parsedHeader = runGet getShpHeader . BL.pack . catMaybes $ hdr
    if (maybe True (boxesOverlap (toRecBB $ shpBB parsedHeader)) boundingBox) then
      conduitDecoder getShpRec =$= CC.map ((,) $ parsedHeader)
    else
      return ()

maybeBoxesOverlap :: Maybe RecBBox -> Maybe RecBBox -> Bool
maybeBoxesOverlap Nothing _ = True
maybeBoxesOverlap _ Nothing = True
maybeBoxesOverlap (Just b1) (Just b2) = boxesOverlap b1 b2

boxesOverlap :: RecBBox -> RecBBox -> Bool
boxesOverlap b1 b2 =  recXMin b1 < recXMax b2 && recXMax b1 > recXMin b2 &&
  recYMin b1 < recYMax b2 && recYMax b1 > recYMin b2

shpDbfConduit :: (MonadIO m, MonadThrow m) => Maybe RecBBox -> Source m ByteString -> Source m ByteString -> Source m (ShpHeader, ShpRec, DbfRow)
shpDbfConduit bbox shp dbf = zipSources (shp =$= shapefileConduit bbox) (dbf =$= dbfConduit)
  =$= CC.map (\((a, b), c) -> (a, b, c))
  =$= CC.filter (\(a, b, c) -> maybeBoxesOverlap (shpRecBBox b) bbox)

shpDbfSource :: Maybe RecBBox -> FilePath -> Source (ResourceT IO) Shape
shpDbfSource bbox filePath = shpDbfConduit bbox shp dbf
   where
      shp = sourceWithExtension filePath "shp"
      dbf = sourceWithExtension filePath "dbf"

sourceWithExtension :: FilePath -> String -> Source (ResourceT IO) ByteString
sourceWithExtension filePath ext = CC.sourceFile (filePath `replaceExtension` ext)

shapesFromDbfShpSource :: Maybe RecBBox -> Handle -> Handle -> Source IO Shape
shapesFromDbfShpSource bbox shp dbf = shpDbfConduit bbox (sourceFromStart shp) (sourceFromStart dbf)

shapesByName :: FilePath -> (Text -> Bool) -> Text -> IO [Shape]
shapesByName filePath columnRule fieldValue = runResourceT $ do
  shpDbfSource Nothing filePath
    =$= CC.filter (matchTextDbfField columnRule fieldValue)
    $$ CC.sinkList

shapeFieldByColumnNameRule :: (Text -> Bool) -> DbfRow -> DbfField
shapeFieldByColumnNameRule rule (DbfRow c) = snd . head $ filter (\(l,_) -> rule $ dbfcName l) c

columnHasCharacter :: Text -> DbfField -> Bool
columnHasCharacter c (DbfFieldCharacter d) = c == d
columnHasCharacter _ _ = False

columnHasNumber :: Int -> DbfField -> Bool
columnHasNumber n (DbfFieldNumeric m) = n == m
columnHasNumber _ _ = False

matchTextDbfField :: (Text -> Bool) -> Text -> Shape -> Bool
matchTextDbfField checkColumn t (_, _, s) = columnHasCharacter t (shapeFieldByColumnNameRule checkColumn s)

matchNumericDbfField :: (Text -> Bool) -> Int -> Shape -> Bool
matchNumericDbfField checkColumn n (_, _, s) = columnHasNumber n (shapeFieldByColumnNameRule checkColumn s)

