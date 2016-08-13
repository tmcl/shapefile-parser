{-# LANGUAGE OverloadedStrings #-}
module Data.Dbase.Conduit
where

import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA

import Data.Dbase.Parser

dbfConduit :: ConduitM ByteString (CA.PositionRange, DbfRow) (ResourceT IO) ()
dbfConduit = 
   CA.sinkParser parseDbfHeader >>= 
   CA.sinkParser . parseDbfColumns >>= 
   CA.conduitParser . parseDbfRow
