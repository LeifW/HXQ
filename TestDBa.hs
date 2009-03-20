{--------------------------------------------------------------
-
- A main program to test XQuery connected to a Sqlite database
- To create the database load the file data/company.sql in sqlite3
-
- Programmer: Leonidas Fegaras (fegaras@cse.uta.edu)
- Date: 12/14/2008
-
---------------------------------------------------------------}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.XML.HXQ.XQuery


main = do _db <- connect "hxq"
          e <- [$qx|   for $r in sql('  select e.fname, d.dname
                                        from employee e, department d
                                        where e.dno = d.dnumber
                                          and e.lname = ?           ',
                                     'English')
                       return <result>{$r/fname,$r/dname}</result>   |]
          putXSeq e
