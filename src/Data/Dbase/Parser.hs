module Data.Dbase.Parser (
   parseDbf,
   Dbf (..),
   DbfColumn (..),
   DbfRow(..),
   DbfField(..),
   parseDbfHeader,
   parseDbfColumns,
   DbfHeader(..),
   parseDbfRow)
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as P
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Binary
import Data.Time.Calendar
import Data.Text.Encoding

data Dbf = Dbf {
   dbfHeader :: DbfHeader,
   dbfColumns :: [DbfColumn],
   dbfData :: [DbfRow]
}
   deriving (Show, Eq)

data DbfHeader = DbfHeader {
   dbfRecordsLength :: Int,
   dbfFirstRecordPosition :: Int
}
   deriving (Show, Eq)

data DbfColumn = DbfColumn {
   dbfcName :: T.Text,
   dbfcType :: DbfColumnType,
   dbfcLength :: Int
}
   deriving (Show, Eq)

data DbfColumnType
   = DbfColumnTypeCharacter
   | DbfColumnTypeDate
   | DbfColumnTypeFloat
   | DbfColumnTypeLogical
   | DbfColumnTypeMemo
   | DbfColumnTypeNumeric
   deriving (Eq, Show)

data DbfField
   = DbfFieldCharacter {dbfFieldCharacter :: T.Text}
   | DbfFieldDate (Maybe Day)
   | DbfFieldFloat ByteString
   | DbfFieldLogical Bool
   | DbfFieldMemo ByteString
   | DbfFieldNumeric Int
   deriving (Eq, Show)

data DbfRow = DbfRow [(DbfColumn, DbfField)]
   deriving (Eq, Show)

parseDbf :: Parser Dbf
parseDbf = do
   hdr <- parseDbfHeader
   columns <- parseDbfColumns hdr
   rows <- parseDbfRows hdr columns
   let r = Dbf hdr columns rows
   return r

parseDbfHeader :: Parser DbfHeader
parseDbfHeader = do
   _ <- P.take 4
   len <- anyWord32le
   firstPosition <- anyWord16le
   _ <- P.take 22
   return $ DbfHeader (fromIntegral len) (fromIntegral firstPosition)

readColumnType :: ByteString -> DbfColumnType
readColumnType "C" = DbfColumnTypeCharacter
readColumnType "F" = DbfColumnTypeFloat
readColumnType "D" = DbfColumnTypeDate
readColumnType "N" = DbfColumnTypeNumeric
readColumnType "M" = DbfColumnTypeMemo
readColumnType "L" = DbfColumnTypeLogical
readColumnType l = error $ "Unknown column type" ++ (show l)

parseDbfColumn :: Parser DbfColumn
parseDbfColumn = do
   name <- parseUtf8 11
   columnType <- readColumnType <$> P.take 1
   _ <- P.take 4
   len <- anyWord8
   _ <- P.take 15
   return $ DbfColumn name columnType (fromIntegral len)

parseDbfColumns :: DbfHeader -> Parser [DbfColumn]
parseDbfColumns header = do
   r <- count ((fromIntegral . dbfFirstRecordPosition) header `div` 32 - 1) parseDbfColumn
   _ <- P.take 1
   return r

parseDbfRows :: DbfHeader -> [DbfColumn] -> Parser [DbfRow]
parseDbfRows header columns
   = count (fromIntegral . dbfRecordsLength $ header) (parseDbfRow columns)

parseDbfRow :: [DbfColumn] -> Parser DbfRow
parseDbfRow columns = do
   _ <- anyWord8
   fields <- mapM parseDbfField columns
   return $ DbfRow fields

parseDbfField :: DbfColumn -> Parser (DbfColumn, DbfField)
parseDbfField column = fmap (\l -> (column, l)) (parseField (fromIntegral . dbfcLength $ column))
   where
      parseField len = case dbfcType column of
         DbfColumnTypeCharacter -> DbfFieldCharacter <$> parseUtf8 len
         DbfColumnTypeDate -> DbfFieldDate <$> parseDay len
         DbfColumnTypeFloat -> DbfFieldFloat <$> P.take len
         DbfColumnTypeLogical -> DbfFieldLogical <$> parseBool len
         DbfColumnTypeMemo -> DbfFieldMemo <$> P.take len
         DbfColumnTypeNumeric -> DbfFieldNumeric <$> parseInt len

parseInt :: Int -> Parser Int
parseInt len = do
   num <- P.take len
   return $ floor (readBs (B8.filter (/= ' ') num) :: Double)

readBs :: Read r => ByteString -> r
readBs = read . T.unpack . decodeUtf8

parseUtf8 :: Int -> Parser Text
parseUtf8 len = T.strip . decodeUtf8 . (BS.filter (/= 0)) <$> P.take len

parseDay :: Int -> Parser (Maybe Day)
parseDay 8 = do
   year <- P.take 4
   month <- P.take 2
   day <- P.take 2
   if (month == "00" || day == "00") then
      return $ Nothing
   else
      return $ Just $ fromGregorian (readBs year) (readBs month) (readBs day)
parseDay l = error $ "unsupported day length " ++ (show l)

parseBool :: Int -> Parser Bool
parseBool len = do
   logical <- anyWord8
   _ <- P.take (len - 1)
   return $ inClass "YyTt" logical
