{-# LANGUAGE OverloadedStrings #-}
module Data.Dbase.Conduit (dbfConduit, module Data.Dbase.Parser)
where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Attoparsec as CA

import Data.Dbase.Parser


dbfConduit :: (MonadIO m, MonadThrow m) => ConduitM ByteString DbfRow m ()
dbfConduit = do
   hdr <- CA.sinkParser parseDbfHeader
   columns <- CA.sinkParser . parseDbfColumns $ hdr
   (CA.conduitParser . parseDbfRow) columns =$= CC.take (dbfRecordsLength hdr) =$= CC.map snd
