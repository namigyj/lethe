{-# LANGUAGE OverloadedStrings #-}
module MainSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Mocks
import Main
import Lib

mainSpec :: SpecWith a
mainSpec = do
  describe "Main/run/cat" $
    prop "add"
    (\(rs, cat) ->
       let cmd = catToCmd cat Add
       in runCommand cmd rs
          `shouldBe` Right (addCategory cat rs))
    prop "mod"
    (\(rs,cat) ->
       let cmd = (catTocmd cat Mod){catname=cn}
           cn = _name cat
       in runCommand cmd rs
          `shouldBe` Right (modCategory cn cat rs))
  describe "Main/run/link" $
    prop "add"
    (\(rs,it) ->
       let cmd = itToCmd it Add
       in runCommand cmd rs
          `shouldBe` Right [])
    prop "mod"
    (\(rs,it) ->
       let cmd = itToCmd it Mod
       in runCommand cmd rs
          `shouldBe` Right [])
    prop "del"
    (\(rs,it) ->
       let cmd = itToCmd it Del
       in runCommand cmd rs
          `shouldBe` Right [])

catToCmd :: Category -> Method -> Command
catToCmd Category{_name,_description} m = Cat{newname=_name, catname=""
                                             ,description=_description, method=m}

itToCmd :: Cateogry -> Method -> String -> Command
itToCmd Item{_title,_url,_tags,_uid} m = Link{title=_title, url=_url, tags=_tags
                                             ,uid=_uid, method=m, catname="Articles"}
