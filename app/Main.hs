{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.CmdArgs

import FtpIO
import qualified Lib as L

main :: IO ()
main = (print =<< mode) >>
       (readLinks testReadLocation) >>= (writeLinks testWriteLocation . L.computeUids)

data Method = Add | Mod | Del
              deriving (Data, Typeable, Show, Eq)
data Lethe
  = Link { title::String, url::String, tags::String
         , uid::Maybe String, method::Method, catname::Maybe String }
  | Cat  { name_::String, description::String
         , method::Method, catname::Maybe String }
  deriving (Data, Typeable, Show, Eq)

link :: Lethe
link = Link
  { catname = Just "Articles" &= help "Category name to add the link to" &= typ "STR"
  , title = def &= help "Title of the resource" &= name "t" &= typ "STR"
  , url = def &= help "Url of the resource" &= name "u" &= typ "STR"
  , tags = def &= help "comma separated list of tags" &= typ "ITEMS"
  , method = enum
        [ Add &= help "Add a new link"
        , Mod &= help "Modify an existing link"
        , Del &= help "Delete a link" &= name "del"]
  , uid = def &= args &= typ "UID"
  } &= help "Add a new link to a category" &= auto

cat :: Lethe
cat = Cat
  { name_ = def &= help "New name of the category" &= name "n" &= typ "STR"
  , description = def &= help "New description" &= name "d" &= typ "STR"
  , method = enum
        [ Mod &= help "Modify an existing category"
        , Add &= help "Add a new category"]
  , catname = def &= args &= typ "CATEGORY_NAME" -- &= opt "Articles"
  }

mode :: IO Lethe
mode = cmdArgs $ modes [link,cat]
  &= help "Add manage links on dfc.moe/links"
  &= program _PROGRAM_NAME
  &= summary (_PROGRAM_NAME ++ _PROGRAM_VERSION ++ "\nRecord it and never forgetti")
  &= helpArg [explicit, name "help", name "h"]


-- cmdHandler should be :: Lethe -> (ResultSet -> ResultSet)
-- that way it only gives back an operation, independant of the data we would apply on it
cmdHandler :: Lethe -> L.ResultSet -> L.ResultSet
cmdHandler Link{method=Mod, uid=Nothing} = badArg "id of the link required"
cmdHandler Link{method=Del, uid=Nothing} = badArg "id of the link required"
cmdHandler Link{method=Add, catname=Nothing} = badArg "need a category name to add the Link to"
cmdHandler Link{method=Add, title=t, url=u, catname=Just cn, tags=tg}
  = if any (=="") [t,u]
    then badArg "title and url are required"
    else L.appendItem cn L.Item{title=t, url=u, tags=splitTags tg}
cmdHandler Link{method=Mod, uid=Just id', title=t, url=u, tags=tg}
  = L.modItem id' L.Item{title=t, url=u, tags=splitTags tg}
cmdHandler Link{method=Del, uid=Just id'}
  = L.deleteItem id'

-- addTags method or is modItem enough ?
cmdHandler Cat{method=Mod, catname=Nothing} = badArg "CATEGORY_NAME of the target is required"
cmdHandler Cat{method=Add, name_=n, description=d}
  = if any (=="") [n,d]
    then badArg "name and description are required"
    else L.appendCategory (L.Category n d [])

cmdHandler Cat{method=Mod, catname=Just cn, name_=n, description=d}
  = L.modCategory cn (L.Category n d [])
cmdHandler Cat{method=Del} = badArg "forbidden to delete a category"

badArg :: String -> a
badArg msg = error $ "bad argument: " ++ msg

_PROGRAM_NAME = "lethe"
_PROGRAM_VERSION = "v0.1"
_PROGRAM_NAME :: String
_PROGRAM_VERSION :: String

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
  in
    split ';'
