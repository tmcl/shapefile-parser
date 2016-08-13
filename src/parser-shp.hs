module Main
where

import qualified Data.ByteString as BS
import Geometry.Shapefile.ReadShp
import System.Environment
import Text.Show.Pretty

main :: IO ()
main = getArgs >>= mapM_ testParse

testParse :: FilePath -> IO ()
testParse filePath = readShpFile filePath >>= parseTest

--parseTest :: Show v => Parser v -> BS.ByteString -> IO ()
parseTest = (putStrLn . ppShow)
