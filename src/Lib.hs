{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Lib ( ResultSet (..)
           , Category (..)
           , Item (..)
           , computeAllUids
           , computeuid
           , computeuid'
           , addItem
           , modItem
           , delItem
           , addCategory
           , modCategory
           , splitTags
           ) where

import GHC.Word (Word16)
import System.Console.CmdArgs (Data, Typeable)

import Lens.Micro
import Lens.Micro.TH (makeLenses)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | list of all categories
newtype ResultSet = ResultSet { _cats :: [Category]
                              } deriving (Eq, Show)

-- | category of bookmark links
data Category = Category { _name :: !String        -- ^ name of the Category
                         , _description :: !String -- ^ description of it
                         , _items :: [Item]        -- ^ collection of bookmark links
                         } deriving (Eq, Show)

-- | a bookmark item
data Item = Item { _title :: !String  -- ^ The title/name of the bookmark link
                 , _url :: !String    -- ^ URL
                 , _tags :: ![String] -- ^ List of tags (for later being able to query)
                 , _uid :: !Word16    -- ^ not so unique identifier, (hash of title)
                 } deriving (Eq, Show)

makeLenses ''ResultSet
makeLenses ''Category
makeLenses ''Item

-- | re-computes all the uids of Items in the entire ResultSet
-- | FIXME : make sure they are unique
computeAllUids :: ResultSet -> ResultSet
computeAllUids = cats.each.items.each %~ computeuid

-- | update an item's uid
computeuid :: Item -> Item
{-# WARNING computeuid "TODO" #-}
computeuid = id

-- | compute the uid of an item
computeuid' :: Item -> Word16
{-# WARNING computeuid' "TODO" #-}
computeuid' = _uid

-- | append a new category
addCategory :: Category -> ResultSet -> ResultSet
addCategory = (cats <>~) . pure

-- | modify the category with the given name
modCategory :: String -> Category -> ResultSet -> ResultSet
modCategory cname cat = cats.each.filtered (\c -> _name c == cname) %~ (cat ??)

-- | append a new Item to the given name's Category items
addItem :: String -> Item -> ResultSet -> ResultSet
addItem cname i = cats . singular each.filtered (\c -> _name c == cname) . items <>~ [i]

-- | delete all items with given uid from all categories
-- | FIXME fail and/or warn if two items have same uid
delItem :: Word16 -> ResultSet -> ResultSet
delItem uid = cats.each.items %~ filter (\i -> _uid i /= uid)

-- | update the _first_ item with the same uid
-- | FIXME fail and/or warn if two items have same uid
modItem :: Item -> ResultSet -> ResultSet
modItem it = cats.each . items.singular each. filtered (\i -> _uid i == _uid it) %~ (it ??)

-- | FIXME : better name
-- | instance for Item also recompute the uids
class Default a where
-- | if lhs is default return rhs
  (??) :: a -> a -> a
instance Default [a] where
  [] ?? ys = ys
  xs ?? _  = xs
instance Default Item where
  i1 ?? i2 = computeuid $ Item{ _title = _title i1 ?? _title i2
                              , _url   = _url i1 ?? _url i2
                              , _tags  = _tags i1 ?? _tags i2
                              , _uid   = 0
                              }
instance Default Category where
  c1 ?? c2 = Category{ _name = _name c1 ?? _name c2
                     , _description = _description c1 ?? _description c2
                     , _items = _items c1 ?? _items c2
                     }

-- | split on Char
splitTags :: String -> [String]
splitTags = splitOn ','

-- | FIXME please rewrite me I'm shit.
splitOn :: Char -> String -> [String]
splitOn c = go [] ""
  where
    go !s "" "" = s
    go s e ""  = s <> [reverse e]
    go s e (c:xs) = case c of
      ',' -> go (s <> [reverse e]) "" xs
      ' ' -> go s ('_':e) xs
      _   -> go s (c:e) xs

-- tests
x,y,z :: Item
x = Item "i_x" "" ["ta","tb"] 0
y = Item "i_y" "" ["tb"] 1
z = Item "i_z" "" ["ta"] 2
a,b,c :: Category
a = Category "a" "foo_a" [x,y,z]
b = Category "b" "foo_b" [Item "i1" "" [] 3]
c = Category "b" "foo_b" [x, Item "i2" "" [] 4]
r :: ResultSet
r = ResultSet [a,b,c]
