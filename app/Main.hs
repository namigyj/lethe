{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import FtpIO
import CmdLethe
import qualified Lib as L

-- DELETE ME

main :: IO ()
main = do md <- mode
          print md
          rs <- readLinks tmpTest
          execOrQuit (cmdHandler md) rs

  -- There must be a better way to do Error handling, but I have no idea ...
execOrQuit :: Either (L.ResultSet -> L.ResultSet) (IO ()) -> L.ResultSet -> IO ()
execOrQuit (Left f) rs = writeLinks tmpTest $ f rs
execOrQuit (Right err) _ = err

-- cmdHandler should be :: Lethe -> (ResultSet -> ResultSet)
-- that way it only gives back an operation, independant of the data we would apply on it
cmdHandler :: Lethe -> Either (L.ResultSet -> L.ResultSet) (IO ())
cmdHandler Link{method=Mod, uid=Nothing} = Right $ badArg "id of the link required"
cmdHandler Link{method=Del, uid=Nothing} = Right $ badArg "id of the link required"
cmdHandler Cat{method=Del} = Right $ badArg "forbidden to delete a category"
cmdHandler Cat{method=Mod, catname=Nothing} = Right $ badArg "CATEGORY_NAME of the target is required"
-- default catname
cmdHandler ln@Link{method=Add, catname=Nothing}
    = cmdHandler ln{catname=Just "Articles"}
cmdHandler Link{method=Add, title=t, url=u, catname=Just cn, tags=tg}
    = if any (=="") [t,u]
      then Right $ badArg "title and url are required"
      else Left $ L.appendItem cn (L.Item "" t u $ splitTags tg)

cmdHandler Link{method=Mod, uid=Just id', title=t, url=u, tags=tg}
    = Left $ L.modItem id' (L.Item "" t u $ splitTags tg)

cmdHandler Link{method=Del, uid=Just id'}
    = Left $ L.deleteItem id'

cmdHandler Cat{method=Add, name_=n, description=d}
    = if any (=="") [n,d]
      then Right $ badArg "name and description are required"
      else Left $ L.appendCategory (L.Category n d [])

cmdHandler Cat{method=Mod, catname=Just cn, name_=n, description=d}
    = Left $ L.modCategory cn (L.Category n d [])
cmdHandler Refresh
    = Left $ L.computeAllUids


badArg :: String -> IO ()
badArg msg = die $ "[Error] Bad argument: " ++ msg

-- Tags is a ','-separated list in a string
splitTags :: String -> [String]
splitTags =
  let
    split2 s str = iter "" str
        where
            iter acc []     = (acc, "")
            iter acc (x:xs) | x == s    = (acc, xs)
                            | otherwise = iter (acc++[x]) xs

    split s str = iter [] str
        where
            iter acc [] = acc
            iter acc xs = iter (acc++[hd]) tl
                where (hd, tl) = split2 s xs
    emptyOrWhiteSpace s = (=="") s || all (==' ') s
  in
    filter (not . emptyOrWhiteSpace) . split ','
