{-# LANGUAGE OverloadedStrings #-}
module Mocks where

import Data.Text (pack)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Lib

x,y,z,v,w :: Item
v = Item "i_v" "" [] 3
w = Item "i_w" "" [] 4
x = Item "i_x" "" ["ta","tb"] 0
y = Item "i_y" "" ["tb"] 1
z = Item "i_z" "" ["ta"] 2

a',b',c' :: Category
a = Category "name_a" "descr_a"
a' = a [x,y,z]
b = Category "name_b" "descr_b"
b' = b [v]
c = Category "name_c" "descr_c"
c' = c [x,w]

r :: ResultSet
r = ResultSet [a',b',c']

instance Arbitrary Item where
  arbitrary = do
    title <- pack <$> arbitrary
    url <- pack <$> arbitrary
    n <- arbitrary
    tags <- fmap pack . take (n `mod` 10) <$> arbitrary
    -- who cares about modulo bias :^)
    Item title url tags <$> arbitrary

instance Arbitrary Category where
  arbitrary = do
    name <- pack <$> arbitrary
    descr <- pack <$> arbitrary
    n <- arbitrary
    -- who cares about modulo bias :^)
    Category name descr . take (n `mod` 40) <$> arbitrary

instance Arbitrary ResultSet where
  arbitrary = ResultSet <$> arbitrary
