module FtpIO where

import Lib

import           Data.Aeson
import qualified Data.ByteString       as B  (readFile)
import qualified Data.ByteString.Lazy  as BL
--import           Data.ByteString.Char8       (unpack, pack)
import System.IO()


testReadLocation :: FilePath
testReadLocation = "/home/zerotsu/code/haskell/lethe/links.json"

testWriteLocation :: FilePath
testWriteLocation = "/home/zerotsu/code/haskell/lethe/test.json"

someFunc :: IO ()
someFunc = do str <- B.readFile testReadLocation
              let
                set = (getSet . BL.fromStrict) str
                in (print . show . encode . computeAllUids) set

writeLinks :: FilePath -> ResultSet -> IO ()
writeLinks filepath rs = do BL.writeFile filepath . encode $ rs

readLinks :: FilePath -> IO ResultSet
readLinks filepath = do str <- B.readFile filepath
                        return . getSet. BL.fromStrict $ str

getSet :: BL.ByteString -> ResultSet
getSet str = case (decode str) of
  Just x -> x
  Nothing -> ResultSet []
