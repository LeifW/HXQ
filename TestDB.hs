{--------------------------------------------------------------
-
- A main program to test XQuery connected to a Sqlite database
- To create the database load the file data/company.sql in sqlite3
-
- Programmer: Leonidas Fegaras (fegaras@cse.uta.edu)
- Date: 07/24/2008
-
---------------------------------------------------------------}

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Text.XML.HXQ.XQuery


main = do db <- connect "hxq"
          e <- $(xqdb ("  for $r in sql('  select e.fname, d.dname          \
                     \                     from employee e, department d    \
                     \                     where e.dno = d.dnumber          \
                     \                       and e.lname = ?           ',   \
                     \                  'English')                          \
                     \    return <result>{$r/fname,$r/dname}</result>       ")) db
          putXSeq e
