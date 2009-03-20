module Main where

import Text.XML.HXQ.XQuery


main = do db <- connect "hxq"
          -- Generate the relational schema for the DBPL database using hybrid inlining. Ignore HTML tags
          genSchema db "data/dblp.xml" "d" ["i","sub","sup","tt"]
