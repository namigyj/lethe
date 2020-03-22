{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib ( ResultSet (..)
           , Category (..)
           , Item (..)
           , computeAllUids
           , computeuid
           , computeuid'
           , addCategory
           , modCategory
           , addItem
           , modItem
           , delItem
           , splitTags
           ) where

import           Data.Char (isSpace)
import           GHC.Generics (Generic)
import           GHC.Word (Word16)

import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import           Data.Hashable (hash)
import           Data.Text (Text, splitOn, strip)
import qualified Data.Text as T
import           Lens.Micro
import           Lens.Micro.TH (makeLenses)

-- | list of all categories
newtype ResultSet = ResultSet { _cats :: [Category]
                              } deriving (Eq, Show, Generic)

-- | category of bookmark links
data Category = Category { _name :: !Text        -- ^ name of the Category
                         , _description :: !Text -- ^ description of it
                         , _items :: [Item]      -- ^ collection of bookmark links
                         } deriving (Eq, Show, Generic)

-- | a bookmark item
data Item = Item { _title :: !Text  -- ^ The title/name of the bookmark link
                 , _url :: !Text    -- ^ URL
                 , _tags :: ![Text] -- ^ List of tags (for later being able to query)
                 , _uid :: !Word16  -- ^ not so unique identifier, (hash of title)
                 } deriving (Eq, Show, Generic)

makeLenses ''ResultSet
makeLenses ''Category
makeLenses ''Item
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ResultSet
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Category
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Item

-- | re-computes all the uids of Items in the entire ResultSet
-- | @FIXME : make sure they are unique (?)
computeAllUids :: ResultSet -> ResultSet
computeAllUids = cats.each.items.each %~ computeuid

-- | update an item's uid
computeuid :: Item -> Item
computeuid i = i{_uid=computeuid' i}

-- | compute the uid of an item
computeuid' :: Item -> Word16
computeuid' Item{_title,_url,_tags} = fromIntegral $ hash (_title,_url,_tags)

-- | append a new category
addCategory :: Category -> ResultSet -> ResultSet
addCategory = (cats <>~) . pure

-- | modify the category with the given name
modCategory :: Text -> Category -> ResultSet -> ResultSet
modCategory cname cat = cats.each.filtered (\c -> _name c == cname) %~ (cat ??)

-- | append a new Item to the given name's Category items
addItem :: Text -> Item -> ResultSet -> ResultSet
addItem cname i = cats . singular each.filtered (\c -> _name c == cname) . items <>~ [i]

-- | delete all items with given uid from all categories
-- | @FIXME fail and/or warn if two items have same uid
delItem :: Word16 -> ResultSet -> ResultSet
delItem uid = cats.each.items %~ filter (\i -> _uid i /= uid)

-- | update the _first_ item with the same uid
-- | @FIXME fail and/or warn if two items have same uid
modItem :: Item -> ResultSet -> ResultSet
modItem it = cats.each . items.singular each. filtered (\i -> _uid i == _uid it) %~ (it ??)

-- * Helpers
-- | split on Char
splitTags :: Text -> [Text]
splitTags = fmap (unspace . strip) . splitOn ","
  where
    unspace = T.map (\c -> if isSpace c then '_' else c)

-- | instance for Item also recompute the uids
class Null a where
  (??) :: a -> a -> a
instance Null [a] where
  [] ?? ys = ys
  xs ?? _  = xs
instance Null Text where
  "" ?? ys = ys
  xs ?? _  = xs
instance Null Item where
  i1 ?? i2 = computeuid $ Item{ _title = _title i1 ?? _title i2
                              , _url   = _url i1 ?? _url i2
                              , _tags  = _tags i1 ?? _tags i2
                              , _uid   = 0
                              }
  -- ^  if lhs is default return rhs
instance Null Category where
  c1 ?? c2 = Category{ _name = _name c1 ?? _name c2
                     , _description = _description c1 ?? _description c2
                     , _items = _items c1 ?? _items c2
                     }
