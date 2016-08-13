{-# LANGUAGE OverloadedStrings #-}
module Data.Shapefile.Conduit
where

import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA

import Data.Shapefile.Parser

-- main = getArgs >>= mapM_ (\fp -> runResourceT $ CB.sourceFile fp =$= dbfConduit $$ CC.mapM_ (liftIO . putStrLn . ppShow))

dbfConduit :: ConduitM ByteString (CA.PositionRange, DbfRow) (ResourceT IO) ()
dbfConduit = do
   hdr <- CA.sinkParser parseDbfHeader
   columns <- CA.sinkParser $ parseDbfColumns hdr
   CA.conduitParser $ parseDbfRow columns
