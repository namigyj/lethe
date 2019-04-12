{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Data.Text (Text, breakOn, drop, strip, lines, unpack, intercalate)
import Data.Text.IO (readFile)
import Lens.Micro
import Lens.Micro.Internal (Each(..))
import Lens.Micro.TH (makeLenses)
import Prelude hiding (drop, readFile, lines)
import System.Directory (getXdgDirectory, XdgDirectory(..))

data Config = Config { _hostn :: String -- ^ hostname
                     , _uname :: String -- ^ username
                     , _passw :: String -- ^ password
                     , _fpath :: String -- ^ filepath
                     } deriving (Eq, Show)

makeLenses ''Config

configFileName :: FilePath
configFileName = "lethe.conf"

configFilePath :: IO FilePath
{-# WARNING configFilePath "FIXME" #-}
configFilePath = getXdgDirectory XdgConfig configFileName

getConfig :: ExceptT String IO Config
getConfig = lift (configFilePath >>= readFile) >>= except . go
  where
    go input = mkConfig (parseConfigFile input) >>= anyNull

mkConfig :: [(Text, Text)] -> Either String Config
mkConfig [] = Left "Empty config."
mkConfig xs = Right . go (Config "" "" "" "") . map (fmap unpack) $ xs
  where
    go c ((k, v):xs) = let c' | k == "hostname" = c{_hostn=v}
                              | k == "username" = c{_uname=v}
                              | k == "password" = c{_passw=v}
                              | k == "filepath" = c{_fpath=v}
                              | otherwise       = c
                       in go c' xs

anyNull :: Config -> Either String Config
anyNull c | null missing = Right c
          | otherwise = Left . unpack . intercalate "," $ missing
  where
    missing = foldr (\(f,e) es -> if null (f c) then e:es else es) [] fields
    fields = [(_hostn,"hostname"),(_uname,"username"),(_passw,"password"),(_fpath,"filepath")]

parseConfigFile :: Text -> [(Text, Text)]
parseConfigFile = map splitAndTrimT . lines

splitAndTrimT :: Text -> (Text, Text)
splitAndTrimT s = let (k, v) = drop 1 <$> breakOn "=" s
                  in (strip k,strip v)
