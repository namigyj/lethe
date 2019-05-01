{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Text.Read (readMaybe)
import Data.Word (Word16)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Data.Aeson (encode, eitherDecodeStrict)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Data.Text (Text,unpack,empty)
import Network.FTP.Client
import System.Console.CmdArgs

import Config
import Lib

-- | How to operate.
data Method = Add | Mod | Del
              deriving (Data, Typeable, Show, Eq)

-- | What to operate on.
data Command
  = Link { title'::Text, url'::Text, tags'::Text
         , uid'::Text, method'::Method, catname'::Text }
  | Cat  { description'::Text, newname'::Text
         , method'::Method, catname'::Text }
  | Refresh
  deriving (Data, Typeable, Show, Eq)

-- cmdargs link mode
link :: Command
link = Link
  { catname' = "Articles" &= name "c"  &= typ "STR"
              &= help "category name to add the link to (default: Articles)"
  , title' = def &= name "t" &= typ "STR"
            &= help "title of the item"
  , url' = def &= name "u" &= typ "STR"
          &= help "url of the item"
  , tags' = def &= typ "LIST" &= name "g"
           &= help "comma (,) separated list of tags"
  , method' = enum
        [ Add &= help "add a new item (default)"
        , Mod &= help "modify an existing item"
        , Del &= help "delete an existing item"
        ]
  , uid' = def &= args &= typ "HEX"
  } &= help "add, modify or delete a single bookmark item" &= auto

-- cmdargs cat mode
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

-- cmdargs refresh mode
refresh :: Command
refresh = Refresh {} &= help "Recompute the uids of all the items"

mode :: IO Command
mode = cmdArgs $ modes [link, cat, refresh]
  &= help "Add manage links on dfc.moe/links"
  &= program "lethe"
  &= summary ("lethe " <> "v0.2" <> "\nRecord it and never forgetti")
  &= helpArg [explicit, name "help", name "h"]

-- | apply command on result set
runCommand :: Command -> ResultSet -> Either String ResultSet
runCommand Link{method',title',url',uid',tags',catname'} =
  let tags = splitTags tags'
      it = computeuid $ Item{_title=title',_url=url',_tags=tags,_uid=0}
      uid = case readMaybe ("0x" <> unpack uid') of
              Nothing -> Left ("Could not parse uid: " <> unpack uid')
              Just x -> Right x
  in case method' of
    Add -> pure . addItem catname' it
    Mod -> pure . modItem it
    Del -> \r -> flip delItem r <$> uid
runCommand Cat{method',catname',newname',description'} =
  let it = Category {_name=newname',_description=description',_items=[]}
  in case method' of
    Add -> pure . addCategory it
    Mod -> pure . modCategory catname' it
runCommand Refresh = pure . computeAllUids

instance Default Text where
  def = empty

main :: IO ()
main = runExceptT main' >>= \case Left s -> putStrLn $ "ERROR " <> s
                                  Right s -> print s

main' :: ExceptT String IO FTPResponse
main' = do com <- lift mode
           Config{_hostn,_uname,_passw,_fpath} <- getConfig
           withFTP _hostn 21 $ \h _ -> do
             s <- login h _uname _passw
             bs <- retr h _fpath
             rs <- except (eitherDecodeStrict bs >>= runCommand com)
             stor h _fpath (toStrict . encode $ rs) TA
             quit h
