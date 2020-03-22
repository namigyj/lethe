{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import System.Environment

import LibSpec
import MainSpec

main :: IO ()
main = do args <- getArgs
          hspec do
            mapM_ snd specs

  where
    specs = [("lib", libSpec)
            ,("main", mainSpec)
            ]
