module FtpIO where

import Lib

import           System.Exit
import           System.IO()
import qualified System.Directory      as SD

import           Data.Aeson
import           Data.ByteString.Lazy.Char8  (pack, unpack)
import           Data.List.Utils             (seqList)
import qualified Data.ByteString.Lazy  as BL

import qualified Network.Curl          as C

import Network.FTP.Client (FTPConnection, easyConnectFTP, login, getlines, putlines)
import Network.FTP.Client.Parser (FTPResult)


-- | Connect to the ftp server after reading config File
ftpconn :: FtpParams -> IO FTPConnection
ftpconn FtpParams{host=h,user=u,pass=p} =
    do con <- easyConnectFTP h
       _ <- login con u (Just p) Nothing
       return con

-- | get the file but with curl, because I can't get around the laziness
-- | despite using seqList
readCurl :: String -> IO ResultSet
readCurl durl' = C.curlGetString durl' []
                >>= return . getSet . pack . snd

-- | get the ResultSet from the JSONfile from the FTPConnection
readFtp :: FTPConnection -> String -> IO ResultSet
readFtp h fl = do s <- getlines h fl
                  return . getSet . pack . unlines $ seqList (fst s)

-- | write the ResultSet to the file on the FtpConnection
  -- .eg. "www/data/links.json"
writeFtp :: FTPConnection -> String -> ResultSet -> IO FTPResult
writeFtp h fl rs = putlines h fl . lines . unpack . encode  $ rs


getSet :: BL.ByteString -> ResultSet
getSet str = case (decode str) of
  Just x -> x
  Nothing -> ResultSet []
-- | Params from the config file to set up the FTPConnection
data FtpParams = FtpParams { host :: String -- | hostname of FTP server
                           , user :: String -- | username on FTP server
                           , pass :: String -- | password on FTP server
                           , floc :: String -- | path of file to write on FTP server
                           , durl :: String -- | url of file to read (cURL)
                           } deriving (Show)

mkParams :: [(String, String)] -> FtpParams
mkParams ls = iter ls $ FtpParams "" "" "" "" ""
  where
    iter [] conf = conf
    iter ((k,v):xs) conf = iter xs cc
      where cc = case (k, v) of
                   ("host"       , _) -> iter xs conf{host=v}
                   ("password"   , _) -> iter xs conf{pass=v}
                   ("username"   , _) -> iter xs conf{user=v}
                   ("ftplocation", _) -> iter xs conf{floc=v}
                   ("dataurl"    , _) -> iter xs conf{durl=v}
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
