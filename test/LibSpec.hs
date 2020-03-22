{-# LANGUAGE OverloadedStrings #-}
module LibSpec where

import Data.Text (pack)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Lib
import Mocks

libSpec = do
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
    it "" $ pendingWith "todo"
  describe "Lib/modItem" $
    it "" $ pendingWith "todo"
  describe "Lib/delItem" $
    it "" $ pendingWith "todo"
