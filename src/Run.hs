{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Run where

import Text.Read (readMaybe)

import Data.Text (Text,unpack)
import System.Console.CmdArgs

import Lib

-- | How to operate.
data Method = Add | Mod | Del
            deriving (Data, Typeable, Show, Eq)

-- | What to operate on.
data Command
  = Link { title::Text, url::Text, tags::Text
         , uid::Text, method::Method, catname::Text }
  | Cat  { description::Text, newname::Text
         , method::Method, catname::Text }
  | Refresh
  deriving (Data, Typeable, Show, Eq)

-- | apply command on result set
runCommand :: Command -> ResultSet -> Either String ResultSet
runCommand Link{method,title,url,uid,tags,catname} =
  let tags' = splitTags tags
      it = computeuid $ Item{_title=title,_url=url,_tags=tags',_uid=0}
      uid' = case readMaybe ("0x" <> unpack uid) of
               Nothing -> Left ("Could not parse uid: " <> unpack uid)
               Just x -> Right x
  in case method of
    Add -> pure . addItem catname it
    -- @REFACTOR
    Mod -> \r -> (\u -> modItem it{_uid=u} r) <$> uid'
    Del -> \r -> flip delItem r <$> uid'
runCommand Cat{method,catname,newname,description} =
  let it = Category {_name=newname,_description=description,_items=[]}
  in case method of -- @FIXME non-exhaustive patmatch
    Add -> pure . addCategory it
    Mod -> pure . modCategory catname it
    Del -> const (Left "Deleting a category is not allowed.")
runCommand Refresh = pure . computeAllUids
