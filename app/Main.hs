module Main where

import FtpIO
import Lib

main :: IO ()
main = do rset <- readLinks
          writeLinks $ computeUids rset;
