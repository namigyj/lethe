{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import FtpIO
import CmdLethe
import qualified Lib as L
import Network.FTP.Client (quit)

main :: IO ()
main = do h <- readConfig >>= ftpconn
          md <- mode
          print md
          rs <- readCurl
          -- TODO : handle Result message (print sth ?)
          cmdHandler md
            >>= \f -> writeFtp h (f rs) -- keep warning for TODO
            >>= print
          quit h
            >>= print


-- cmdHandler should be :: Lethe -> (ResultSet -> ResultSet)
-- that way it only gives back an operation, independant of the data we would apply on it
cmdHandler :: Lethe -> IO (L.ResultSet -> L.ResultSet)
cmdHandler Refresh
    = return $ L.computeAllUids
cmdHandler Link{method=Mod, uid=Nothing} = badArg "id of the link required"
cmdHandler Link{method=Del, uid=Nothing} =  badArg "id of the link required"
cmdHandler Cat{method=Del} = badArg "forbidden to delete a category"
cmdHandler Cat{method=Mod, catname=Nothing} = badArg "CATEGORY_NAME of the target is required"
-- default catname
cmdHandler ln@Link{method=Add, catname=Nothing}
    = cmdHandler ln{catname=Just "Articles"}
cmdHandler Link{method=Add, title=t, url=u, catname=Just cn, tags=tg}
    = if any (=="") [t,u]
      then badArg "title and url are required"
      else return $ L.appendItem cn (L.Item "" t u $ splitTags tg)

cmdHandler Link{method=Mod, uid=Just id', title=t, url=u, tags=tg}
    = return $ L.modItem id' (L.Item "" t u $ splitTags tg)

cmdHandler Link{method=Del, uid=Just id'}
    = return $ L.deleteItem id'

cmdHandler Cat{method=Add, name_=n, description=d}
    = if any (=="") [n,d]
      then badArg "name and description are required"
      else return $ L.appendCategory (L.Category n d [])

cmdHandler Cat{method=Mod, catname=Just cn, name_=n, description=d}
    = return $ L.modCategory cn (L.Category n d [])


badArg :: String -> IO (L.ResultSet -> L.ResultSet)
badArg msg = die $ "[Error] Bad argument: " ++ msg

-- Tags is a ','-separated list in a string
splitTags :: String -> [String]
splitTags =
  let
    split s str = iter [] str
        where
            iter acc [] = acc
            iter acc xs = iter (acc++[hd]) tl
                where (hd, tl) = split2 s xs
    emptyOrWhiteSpace s = (=="") s || all (==' ') s
  in
    filter (not . emptyOrWhiteSpace) . split ','
