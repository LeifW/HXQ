{--------------------------------------------------------------
-
- Testing the  XQuery compiler on a large file
- Download dblp.xml.gz from http://dblp.uni-trier.de/xml/
-   and uncompress it in the data directory.
- Programmer: Leonidas Fegaras (fegaras@cse.uta.edu)
- Date: 12/14/2008
-
---------------------------------------------------------------}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.XML.HXQ.XQuery


main = do a <- [$qx|   <result>{
                          for $x at $i in doc('data/dblp.xml')//inproceedings
                          where $x/author = 'Leonidas Fegaras'
                          return <paper>{ $i, $x/booktitle/text(),
                                          ':', $x/title/text()
                                 }</paper>
                       }</result> |]
          putXSeq a
