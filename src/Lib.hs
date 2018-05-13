{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    where

import Data.Aeson
import Data.Hashable (hash)
import qualified Data.Text as T

import GHC.Generics -- for the default ToJSON


data ResultSet = ResultSet { cats :: [Category]
                           } deriving (Generic, Show)

data Category = Category { name :: String
                         , description :: String
                         , items :: [Item]
                         } deriving (Generic, Show)

data Item = Item { uid :: String
                 , title :: T.Text
                 , url :: String
                 , tags :: [String]
                 } deriving (Generic, Show)

instance ToJSON ResultSet
instance FromJSON ResultSet where
  parseJSON = withObject "ResultSet" $ \x -> ResultSet
    <$> x .: "cats"

instance ToJSON Category
instance FromJSON Category where
  parseJSON = withObject "Category" $ \x -> Category
    <$> x .: "name"
    <*> x .: "description"
    <*> x .: "items"

instance ToJSON Item
instance FromJSON Item where
  parseJSON = withObject "Item" $ \x -> Item
    <$> x .: "uid"
    <*> x .: "title"
    <*> x .: "url"
    <*> x .: "tags"

computeUids :: ResultSet -> ResultSet
computeUids (ResultSet []) = ResultSet []
computeUids (ResultSet cats') =
  ResultSet $ map (\(Category n d items') -> Category n d $ map computeuid items') cats'
    where computeuid (Item _ title' u ts) = Item (hash' title') title' u ts
            where hash' = show . (flip mod) 0xfffff . hash

-- insertCategory :: String -> Catogory -> [Category]
-- modCategory :: String -> [Category] -> [Category]
-- deleteItem :: String -> [Item] -> [Item]
-- modItem :: String -> Item -> [Item]
