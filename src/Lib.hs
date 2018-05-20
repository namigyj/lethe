{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

module Lib
  {--( computeUids
  , insertAfterCategory
  , modCategoryName
  , modCategory
  , deleteItem
  , modItem
  , ResultSet
  , Category
  , Item )--}where

import Data.Aeson
import Data.Hashable (hash)
import Numeric (showHex)
import Data.Text()

import GHC.Generics -- for the default ToJSON


data ResultSet = ResultSet { cats :: [Category]
                           } deriving (Generic, Show)

data Category = Category { name :: String
                         , description :: String
                         , items :: [Item]
                         } deriving (Generic, Show)

-- The uid should become an Int once there's a consequent amount
-- of data (hex -> Int) applied during reading
data Item = Item { uid :: String
                 , title :: String
                 , url :: String
                 , tags :: [String]
                 } deriving (Generic, Show)

instance ToJSON ResultSet
instance FromJSON ResultSet

instance ToJSON Category
instance FromJSON Category

instance ToJSON Item
instance FromJSON Item

  -- TODO : make a second pass to change uids of doubles (append a or b)
-- re-computes all the uids of Items in the entire ResultSet
computeAllUids :: ResultSet -> ResultSet
computeAllUids (ResultSet []) = ResultSet []
computeAllUids (ResultSet cats') =
  ResultSet $ map (\c@Category{items=its} -> c{items= map computeuid its}) cats'

-- Compute the uid of an Item
computeuid :: Item -> Item
computeuid it@Item{title=t} =  it{uid=(hash' t)}
  where hash' = take 5 . (flip showHex) "" . abs . hash
  -- TODO there's a better way to do this (abs and take 5 are like quite ugly) I believe

-- Append a new category to the ResultSet
appendCategory :: Category -> ResultSet -> ResultSet
appendCategory c = cmap $ (flip (++)) [c]

-- Insert a category after the one with the given name
insertCategory :: String -> Category -> ResultSet -> ResultSet
insertCategory cname cat = cmap iter
  where
    iter [] = []
    iter (c@Category{name=n}:cs) | n == cname = c:cat:cs
                                 | otherwise  = c:iter cs

-- Modify the category with the given name
modCategory :: String -> Category -> ResultSet -> ResultSet
modCategory cname new  = cmap $ map (\old@Category{name=n} -> if
                                            | n == cname -> replaceCat old new
                                            | otherwise -> old)
-- Modify the category with the same name
modCategoryNN :: Category -> ResultSet -> ResultSet
modCategoryNN c@Category{name=n} = modCategory n c


-- append a new Item to the given name's Category items
appendItem :: String -> Item -> ResultSet -> ResultSet
appendItem cname it = icmap cname $ (flip (++)) [computeuid it]
-- prepend a new Item to the given name's Category items
prependItem :: String -> Item -> ResultSet -> ResultSet
prependItem cname it = icmap cname $ (:) (computeuid it)

-- TODO : generic method on if uid == thisid
-- add a Tag to the given uid Item
addTags :: String -> [String] -> ResultSet -> ResultSet
addTags uid' ntags =
  imap $ map (\old@Item{uid=id, tags=otags} -> if id == uid'
                                              then replaceItem old old{tags=otags++ntags}
                                              else old)

-- Modify the Item with the given uid
modItem :: String -> Item -> ResultSet -> ResultSet
modItem uid' new = imap $ map (\old@Item{uid=id} -> if id == uid'
                                                   then computeuid $ replaceItem old new
                                                   else old)
-- Deletes the Item with the given uid
deleteItem :: String -> ResultSet -> ResultSet
deleteItem uid' = imap $ filter (\Item{uid=id} -> id /= uid')

-- cmap/imap aren't good names, as we don't necessarily map over it
cmap :: ([Category] -> [Category]) -> ResultSet -> ResultSet
cmap f (ResultSet cs) = ResultSet (f cs)

imap :: ([Item] -> [Item]) -> ResultSet -> ResultSet
imap f = cmap $ map (\c@Category{items=its} -> c{items=f its})

icmap :: String -> ([Item] -> [Item]) -> ResultSet -> ResultSet
icmap cname f =
  cmap $ map (\c@Category{name=n, items=its} -> if n == cname
                                               then c{items=f its}
                                               else c{items=its})

(??) :: Foldable t => t a -> t a -> t a
(??) x y = if null x then y else x

replaceCat :: Category -> Category -> Category
replaceCat (Category oldname olddescr its) (Category newname newdescr _)  =
  Category (newname ?? oldname) (newdescr ?? olddescr) its

replaceItem :: Item -> Item -> Item
replaceItem (Item _ oldtitle oldurl oldtags) (Item _ newtitle newurl newtags) =
  computeuid $ Item "" (newtitle ?? oldtitle)
                       (newurl ?? oldurl)
                       (newtags ?? oldtags)
