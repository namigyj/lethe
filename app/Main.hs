{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import FtpIO
import CmdLethe
import qualified Lib as L
import Network.FTP.Client (quit)

-- I guess this is what they mean when talking about "abusing do notation"
-- although I have no fucking Idea how I would rewrite this in a readable manner
-- with >>=
main :: IO ()
main = do md <- mode
          cc <- readConfig
          rs <- readCurl $ durl cc
          -- TODO : handle Result message (print sth ?)
          h  <- ftpconn cc
          cmdHandler md
            >>= \f -> writeFtp h (floc cc) (f rs) -- keep warning as reminder
            >>= print
          quit h
            >>= print


-- | Commandline arguments handler, returns a function to apply on the resultSet
cmdHandler :: Lethe -> IO (L.ResultSet -> L.ResultSet)
  -- refresh all the ids
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

-- | Die on bad argument
badArg :: String -> IO (L.ResultSet -> L.ResultSet)
badArg msg = die $ "[Error] Bad argument: " ++ msg

-- | Split a string of tags to a list of tags
--   Tags is a ','-separated list in a string
--   .e.g. "foo,bar,baz"
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
