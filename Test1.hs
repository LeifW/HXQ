{--------------------------------------------------------------
-
- A main program to test the XQuery processor
- Programmer: Leonidas Fegaras (fegaras@cse.uta.edu)
- Date: 07/24/2008
-
---------------------------------------------------------------}

{-# LANGUAGE TemplateHaskell #-}


module Main where

import Text.XML.HXQ.XQuery


f(d,s) = $(xq "<student dept='{$d/text()}'>{$s//firstname/text(),$s//lastname/text()}</student>")

main = do a <- $(xq "  for $s in doc('data/cs.xml')//gradstudent           \
                  \     order by $s/gpa descending, $s//lastname           \
                  \     return <student>{$s//firstname/text(),             \
                  \                      $s//lastname/text(),              \
                  \                      $s/gpa/text()}</student>          ")
          putXSeq a
          let query name = $(xq " doc('data/cs.xml')//gradstudent[.//lastname = $name]//firstname ")
          b <- query $(xe " 'Galanis' ")
          putXSeq b
          c <- $(xq "  <good-students>{                                    \
                    \       let $d := doc('data/cs.xml')                   \
                    \       for $s in $d//gradstudent                      \
                    \       where $s/gpa = 4.0                             \
                    \       return f($d//deptname,$s)                      \
                    \   }</good-students>                                  ")
          putXSeq c
          d <- $(xq "    let $d := doc('data/cs.xml')//gradstudent         \
                    \    return ($d/../deptname,count($d))                 ")
          putXSeq d
          putStrLn "Type an XQuery:"
          iquery <- getLine        -- read an XQuery from input
          e <- xquery iquery       -- evaluate it using the XQuery interpreter
          putXSeq e
