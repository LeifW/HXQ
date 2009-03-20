{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.XML.HXQ.XQuery


main = do db <- connect "hxq"
          -- populate the DBPL database (it takes about 12 minutes)
          $(shredC "hxq" "data/dblp.xml" "d")
          -- create an index on author name
          createIndex db "d" "author"
