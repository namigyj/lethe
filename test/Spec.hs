{-# LANGUAGE OverloadedStrings #-}

import Data.Text (pack)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Lib


main :: IO ()
main = hspec $ do
  describe "Lib/addCategory" $ do
    let cat = a []
    it "empty" $ do
      let rs = ResultSet []
      addCategory cat rs `shouldBe` ResultSet [cat]
    it "r" $ do
      let rs = ResultSet [b []]
      addCategory cat rs `shouldBe` ResultSet [b [], cat]
  describe "Lib/modCategory" $ do
    prop "id" $ \cat ->
      let ncat = Category "" "" []
          rs = ResultSet [cat, a']
      in
        modCategory (_name cat) ncat rs `shouldBe` rs
    it "name" $ do
      let ncat = Category "baba" "" []
      let rs = ResultSet [a'{_name="baba"},b',c']
      modCategory "name_a" ncat r `shouldBe ` rs
    it "decription" $ do
      let ncat = Category "" "kljflkasf" []
      let rs = ResultSet [a'{_description="kljflkasf"},b',c']
      modCategory "name_a" ncat r `shouldBe ` rs
    it "both" $ do
      let ncat = Category "baba" "kljflkasf" []
      let rs = ResultSet [a'{_name="baba",_description="kljflkasf"},b',c']
      modCategory "name_a" ncat r `shouldBe ` rs
  describe "Lib/addItem" $
    it "todo" $ const pending
  describe "Lib/modItem" $
    it "todo" $ const pending
  describe "Lib/delItem" $
    it "todo" $ const pending
  describe "Main/run" $
    it "todo" $ const pending

instance Arbitrary Item where
  arbitrary = do
    t <- pack <$> arbitrary
    u <- pack <$> arbitrary
    g <- fmap pack . take 5 <$> arbitrary
    Item t u g <$> arbitrary
instance Arbitrary Category where
  arbitrary = do
    n <- pack <$> arbitrary
    d <- pack <$> arbitrary
    Category n d . take 40 <$> arbitrary
instance Arbitrary ResultSet where
  arbitrary = ResultSet <$> arbitrary


-- mocks
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
