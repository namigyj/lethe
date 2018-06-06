{-# LANGUAGE DeriveDataTypeable #-}

module CmdLethe where

import System.Console.CmdArgs

data Method = Add | Mod | Del
              deriving (Data, Typeable, Show, Eq)
data Lethe
  = Link { title::String, url::String, tags::String
         , uid::Maybe String, method::Method, catname::Maybe String }
  | Cat  { name_::String, description::String
         , method::Method, catname::Maybe String }
  | Refresh
  deriving (Data, Typeable, Show, Eq)

link :: Lethe
link = Link
  { catname = Just "Articles" &= help "Category name to add the link to" &= typ "STR"
  , title = def &= help "Title of the resource" &= name "t" &= typ "STR"
  , url = def &= help "Url of the resource" &= name "u" &= typ "STR"
  , tags = def &= help "comma (,) separated list of tags" &= typ "ITEMS"
  , method = enum
        [ Add &= help "Add a new link"
        , Mod &= help "Modify an existing link"
        , Del &= help "Delete a link" &= name "del"]
  , uid = def &= args &= typ "UID"
  } &= help "Add, modify or delete bookmarks" &= auto

cat :: Lethe
cat = Cat
  { name_ = def &= help "New name of the category" &= name "n" &= typ "STR"
  , description = def &= help "New description" &= name "d" &= typ "STR"
  , method = enum
        [ Mod &= help "Modify an existing category"
        , Add &= help "Add a new category"]
  , catname = def &= args &= typ "CATEGORY_NAME" -- &= opt "Articles"
  } &= help "Add or modify a category of bookmarks"
refresh :: Lethe
refresh = Refresh {} &= help "Recompute the uids of all the items"

mode :: IO Lethe
mode = cmdArgs $ modes [link,cat,refresh]
  &= help "Add manage links on dfc.moe/links"
  &= program _PROGRAM_NAME
  &= summary (_PROGRAM_NAME ++ _PROGRAM_VERSION ++ "\nRecord it and never forgetti")
  &= helpArg [explicit, name "help", name "h"]

_PROGRAM_NAME = "lethe"
_PROGRAM_VERSION = "v0.1"

_PROGRAM_NAME :: String
_PROGRAM_VERSION :: String
