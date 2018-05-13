module FtpIO where

import Lib

import           Data.Aeson
import qualified Data.ByteString       as B  (readFile)
import qualified Data.ByteString.Lazy  as BL
--import           Data.ByteString.Char8       (unpack, pack)
import qualified System.IO                   (FilePath)


testReadLocation :: FilePath
testReadLocation = "/home/zerotsu/code/haskell/lethe/links.json"

testWriteLocation :: FilePath
testWriteLocation = "/home/zerotsu/code/haskell/lethe/test.json"

someFunc :: IO ()
someFunc = do str <- B.readFile testReadLocation
              let
                set = (getSet . BL.fromStrict) str
                in (print . show . encode . computeUids) set

writeLinks :: ResultSet -> IO ()
writeLinks rs = do BL.writeFile testWriteLocation . encode $ rs

readLinks :: IO ResultSet
readLinks = do str <- B.readFile testReadLocation
               return . getSet. BL.fromStrict $ str

getSet :: BL.ByteString -> ResultSet
getSet str = case (decode str) of
  Just x -> x
  Nothing -> ResultSet []