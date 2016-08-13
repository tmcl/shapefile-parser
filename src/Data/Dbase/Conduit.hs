{-# LANGUAGE OverloadedStrings #-}
module Data.Dbase.Conduit (dbfConduit, module Data.Dbase.Parser)
where

import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Attoparsec as CA

import Data.Dbase.Parser

dbfConduit :: ConduitM ByteString DbfRow (ResourceT IO) ()
dbfConduit = do
   hdr <- CA.sinkParser parseDbfHeader
   columns <- CA.sinkParser . parseDbfColumns $ hdr
   (CA.conduitParser . parseDbfRow) columns =$= CC.map snd
