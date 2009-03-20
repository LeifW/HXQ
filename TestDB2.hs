{--------------------------------------------------------------
-
- A main program to test XML shredding and publishing
-
- Programmer: Leonidas Fegaras (fegaras@cse.uta.edu)
- Date: 07/24/2008
-
---------------------------------------------------------------}

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.XML.HXQ.XQuery


main = do db <- connect "hxq"
          res <- $(xqdb ("    for $s in publish('hxq','c')//gradstudent    \
                         \    where $s//lastname='Galanis'                 \
                         \    return $s//gpa      ")) db
          putXSeq res
