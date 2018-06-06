
module FtpIO where

import Lib

import           System.Exit
import           System.IO()
import           Data.Aeson
import           Data.ByteString.Lazy.Char8  (pack, unpack)
import           Data.List.Utils             (seqList)
import qualified Network.Curl          as C
import qualified Data.ByteString       as B  (readFile)
import qualified Data.ByteString.Lazy  as BL
import qualified System.Directory      as SD
import qualified Network.FTP.Client    as NFC
import Network.FTP.Client.Parser (FTPResult)

testLocation :: FilePath
testLocation = "/home/zerotsu/code/haskell/lethe/links.json"

fileLocation :: String
fileLocation = "www/data/links.json"


-- | Connect to the ftp server after reading config File
ftpconn :: FtpParams -> IO NFC.FTPConnection
ftpconn (FtpParams h u p) = do con <- NFC.easyConnectFTP h
                               _ <- NFC.login con u (Just p) Nothing
                               return con

-- | get the file but with curl, because I can't get around the laziness
-- | despite using seqList
readCurl :: IO ResultSet
readCurl = C.curlGetString "https://dfc.moe/data/links.json" []
           >>= return . getSet . pack . snd

-- | get the ResultSet from the JSONfile from the FTPConnection
readFtp :: NFC.FTPConnection -> IO ResultSet
readFtp h = do s <- NFC.getlines h fileLocation
               return . getSet . pack . unlines $ seqList (fst s)

-- | write the ResultSet to the file on the FtpConnection
writeFtp :: NFC.FTPConnection -> ResultSet -> IO FTPResult
writeFtp h rs = NFC.putlines h fileLocation . lines . unpack . encode  $ rs


getSet :: BL.ByteString -> ResultSet
getSet str = case (decode str) of
  Just x -> x
  Nothing -> ResultSet []

data FtpParams = FtpParams { host :: String
                           , user :: String
                           , pass :: String
                           } deriving (Show)

mkParams :: [(String, String)] -> FtpParams
mkParams ls = iter ls $ FtpParams "" "" ""
  where
    iter [] conf = conf
    iter ((k,v):xs) conf = iter xs cc
      where cc = case (k, v) of
                   ("host"     , _) -> iter xs conf{host=v}
                   ("password" , _) -> iter xs conf{pass=v}
                   ("username" , _) -> iter xs conf{user=v}
                   _                -> iter xs conf

split2 :: Char -> String -> (String, String)
split2 s str = iter "" str
    where
        iter acc []     = (acc, "")
        iter acc (x:xs) | x == s    = (acc, xs)
                        | otherwise = iter (acc++[x]) xs

readConfig :: IO FtpParams
readConfig = do exitIfNotExist configFile
                configFile >>= readFile >>= (return . mkParams . parse)

exitIfNotExist :: IO FilePath -> IO ()
exitIfNotExist fpath = do fp <- fpath
                          exist <- fpath >>= SD.doesFileExist
                          if not exist
                            then die $ "file " ++ fp ++ " does not exist"
                            else return ()

configFile :: IO FilePath
configFile = SD.getXdgDirectory SD.XdgConfig "lethe.conf"

parse :: String -> [(String, String)]
parse = (map $ split2 '=') . lines
