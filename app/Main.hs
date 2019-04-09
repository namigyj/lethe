{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs

main :: IO ()
main = print =<< mode

data Method = Add | Mod | Del
              deriving (Data, Typeable, Show, Eq)
data Command
  = Link { title::Maybe String, url::Maybe String, tags::Maybe String
         , uid::Maybe String, method::Method, catname::String }
  | Cat  { description::Maybe String, newname::Maybe String
         , method::Method, catname::String }
  | Refresh
  deriving (Data, Typeable, Show, Eq)

link :: Command
link = Link
  { catname = "Articles" &= name "c"  &= typ "STR"
              &= help "category name to add the link to (default: Articles)"
  , title = def &= name "t" &= typ "STR"
            &= help "title of the item"
  , url = def &= name "u" &= typ "STR"
          &= help "url of the item"
  , tags = def &= typ "LIST" &= name "g"
           &= help "comma (,) separated list of tags"
  , method = enum
        [ Add &= help "add a new item (default)"
        , Mod &= help "modify an existing item"
        , Del &= help "delete an existing item"
        ]
  , uid = def &= args &= typ "UID"
  } &= help "add, modify or delete a single bookmark item" &= auto

cat :: Command
cat = Cat
  { catname = def &= args &= typ "OLD_CATNAME"
  , description = def &= name "d" &= typ "STR"
                  &= help "new category short description"
  , method = enum
        [ Add &= help "Add a new category (default)"
        , Mod &= help "modify an existing category"
        ]
  , newname = def &= typ "STR"
              &= help "new category name"
  } &= help "Add or modify a category of bookmarks"

refresh :: Command
refresh = Refresh {} &= help "Recompute the uids of all the items"

mode :: IO Command
mode = cmdArgs $ modes [link, cat, refresh]
  &= help "Add manage links on dfc.moe/links"
  &= program "lethe"
  &= summary ("lethe " ++ "v0.2" ++ "\nRecord it and never forgetti")
  &= helpArg [explicit, name "help", name "h"]
