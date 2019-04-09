{-# LANGUAGE TemplateHaskell #-}
module Lib where

import GHC.Word (Word16)

import Lens.Micro
import Lens.Micro.TH (makeLenses)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype ResultSet = ResultSet { _cats :: [Category]
                              } deriving (Eq, Show)

-- | category of bookmark links
data Category = Category { _name :: !String         -- ^ name of the Category
                         , _description :: !String  -- ^ description of it
                         , _items :: [Item]        -- ^ collection of bookmark links
                         } deriving (Eq, Show)

-- | The bookmark link struct
-- | FIXME : The uid should become an Int once there's a consequent amount
-- |         of data (hex -> Int) applied during reading, I think
data Item = Item { _title :: !String  -- ^ The title/name of the bookmark link
                 , _url :: !String    -- ^ URL
                 , _tags :: ![String] -- ^ List of tags (for later being able to query)
                 , _uid :: !Word16    -- ^ not so unique identifier, (hash of title)
                 } deriving (Eq, Show)

makeLenses ''ResultSet
makeLenses ''Category
makeLenses ''Item

addItem :: String -> Item -> ResultSet -> ResultSet
addItem cname i = cats . singular each.filtered (\c -> _name c == cname) . items <>~ [i]

delItem :: Word16 -> ResultSet -> ResultSet
delItem uid = cats . each.items %~ filter (\i -> _uid i /= uid)

-- FIXME better name
class Default a where
  (??) :: a -> a -> a

instance Default [a] where
  [] ?? ys = ys
  xs ?? _  = xs

modItem :: Item -> ResultSet -> ResultSet
modItem it = cats.each . items.singular each.filtered (\i -> _uid i == _uid it) %~ id

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
