module Main
where

import Data.Attoparsec.ByteString hiding (parseTest)
import qualified Data.ByteString as BS
import Data.Dbase.Parser
import System.Environment
import Text.Show.Pretty

main :: IO ()
main = getArgs >>= mapM_ testParse

testParse :: FilePath -> IO ()
testParse filePath = BS.readFile filePath >>= parseTest parseDbf

parseTest :: Show v => Parser v -> BS.ByteString -> IO ()
parseTest parser bs = (putStrLn . ppShow) $ parse parser bs
