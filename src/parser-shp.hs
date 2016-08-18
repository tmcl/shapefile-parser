module Main
where

import Geometry.Shapefile.ReadShp
import System.Environment
import Text.Show.Pretty

main :: IO ()
main = getArgs >>= mapM_ testParse

testParse :: FilePath -> IO ()
testParse filePath = readShpFile filePath >>= parseTest

parseTest :: (Show s) => s -> IO ()
parseTest = putStrLn . ppShow
