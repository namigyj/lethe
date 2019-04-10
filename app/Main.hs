{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Text.Read (readMaybe)
import Data.Word (Word16)

import System.Console.CmdArgs

import Lib

main :: IO ()
main = print =<< mode

data Method = Add | Mod | Del
              deriving (Data, Typeable, Show, Eq)
data Command
  = Link { title'::String, url'::String, tags'::String
         , uid'::String, method'::Method, catname'::String }
  | Cat  { description'::String, newname'::String
         , method'::Method, catname'::String }
  | Refresh
  deriving (Data, Typeable, Show, Eq)

link :: Command
link = Link
  { catname' = "Articles" &= name "c"  &= typ "STR"
              &= help "category name to add the link to (default: Articles)"
  , title' = "" &= name "t" &= typ "STR"
            &= help "title of the item"
  , url' = "" &= name "u" &= typ "STR"
          &= help "url of the item"
  , tags' = def &= typ "LIST" &= name "g"
           &= help "comma (,) separated list of tags"
  , method' = enum
        [ Add &= help "add a new item (default)"
        , Mod &= help "modify an existing item"
        , Del &= help "delete an existing item"
        ]
  , uid' = def &= args &= typ "UID"
  } &= help "add, modify or delete a single bookmark item" &= auto

cat :: Command
cat = Cat
  { catname' = def &= args &= typ "OLD_CATNAME"
  , description' = def &= name "d" &= typ "STR"
                  &= help "new category short description"
  , method' = enum
        [ Add &= help "Add a new category (default)"
        , Mod &= help "modify an existing category"
        ]
  , newname' = def &= typ "STR"
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

run :: Command -> ResultSet -> Either String ResultSet
run Refresh = pure . computeAllUids
run Link{method',title',url',uid',tags',catname'} =
  let tags = splitTags tags'
      it = computeuid $ Item{_title=title',_url=url',_tags=tags,_uid=0}
      uid :: Either String Word16
      uid = case readMaybe ("0x"<>uid') of
              Nothing -> Left ("Could not parse uid: " <> uid')
              Just x -> Right x
  in case method' of
    Add -> pure . addItem catname' it
    Mod -> pure . modItem it
    Del -> \r -> flip delItem r <$> uid
run Cat{method',catname',newname',description'} =
  let it = Category {_name=newname',_description=description',_items=[]}
  in case method' of
    Add -> pure . addCategory it
    Mod -> pure . modCategory catname' it
