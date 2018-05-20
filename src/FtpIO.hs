module FtpIO where

import Lib

import           System.Exit
import           System.IO()
import           Data.Aeson
import qualified Data.ByteString       as B  (readFile)
import qualified Data.ByteString.Lazy  as BL
import qualified System.Directory      as SD


testLocation :: FilePath
testLocation = "/home/zerotsu/code/haskell/lethe/links.json"

someFunc :: IO ()
someFunc = readConfig print

writeLinks :: FilePath -> ResultSet -> IO ()
writeLinks filepath rs = do BL.writeFile filepath . encode $ rs

readLinks :: FilePath -> IO ResultSet
readLinks filepath = do str <- B.readFile filepath
                        return . getSet. BL.fromStrict $ str

getSet :: BL.ByteString -> ResultSet
getSet str = case (decode str) of
  Just x -> x
  Nothing -> ResultSet []

data Conf = Conf { host :: String
                 , user :: String
                 , pass :: String
                 } deriving (Show)
-- SD.getXdgDirectory SD.XdgConfig "lethe.conf" -> ~/.config/lethe.conf
-- SD.doesFileExist :: FilePath -> Bool
mkConf :: [(String, String)] -> Conf
mkConf ls = iter ls $ Conf "" "" ""
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

readConfig :: (Conf -> IO ()) -> IO ()
readConfig f = exitIfNotExist configFile
             $ configFile >>= readFile >>= (f . mkConf . parse)

exitIfNotExist :: IO FilePath -> IO () -> IO ()
exitIfNotExist fpath next = do fp <- fpath
                               exist <- fpath >>= SD.doesFileExist
                               if not exist
                                 then die $ "file " ++ fp ++ " not exist"
                                 else next

configFile :: IO FilePath
configFile = SD.getXdgDirectory SD.XdgConfig "lethe.conf"

parse :: String -> [(String, String)]
parse = (map $ split2 '=') . lines
