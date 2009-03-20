{-------------------------------------------------------------------------------------
-
- The XQuery Compiler and Interpreter
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 03/22/08, last update: 01/17/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}

{-# OPTIONS_HADDOCK prune #-}


-- | HXQ is a fast and space-efficient compiler from XQuery (the standard
-- query language for XML) to embedded Haskell code. The translation is
-- based on Haskell templates. It also provides an interpreter for
-- evaluating ad-hoc XQueries read from input or from files
-- and optional database connectivity using HDBC.
-- For more information, look at <http://lambda.uta.edu/HXQ/>.
module Text.XML.HXQ.XQuery (
       -- * The XML Data Representation
       Name, AttList, XTree(..), XSeq, putXSeq,
       -- * The XQuery Compiler
       xq, xe, qx,
       -- * The XQuery Interpreter
       xquery, eval,
       -- * The XQuery Compiler with Database Connectivity
       xqdb,
       -- * The XQuery Interpreter with Database Connectivity
       xqueryDB,
       -- * Shredding and Publishing XML Documents Using a Relational Database
       genSchema, shred, shredC, printSchema, createIndex,
       -- * Other Functions
       XMLEvent(..), connect, disconnect, commit, rollback, prepareSQL, executeSQL
    ) where

import HXML(Name,AttList)
import Text.XML.HXQ.XTree
import Text.XML.HXQ.OptionalDB
import XMLParse
import Text.XML.HXQ.Compiler
import Text.XML.HXQ.Interpreter


-- | The XQuery interpreter as an XQuery function.
eval :: XSeq -> IO XSeq
eval x = case x of
           [ XText q ] -> xquery q
           _ -> error $ "The eval argument must be a string: " ++ show x
