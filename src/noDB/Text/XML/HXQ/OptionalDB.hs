{-------------------------------------------------------------------------------------
-
- No database connectivity
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 08/14/08, last update: 01/14/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}


module Text.XML.HXQ.OptionalDB where

import Text.XML.HXQ.XTree
import Text.XML.HXQ.Parser
import Language.Haskell.TH


type Statement = String
type Table = String

data Connection = Connection String


noDBerror = error "This version of HXQ does not provide database connectivity"


publishXmlDoc :: FilePath -> String -> Bool -> Ast
publishXmlDoc filepath name _ = noDBerror


executeSQL :: Statement -> XSeq -> IO XSeq
executeSQL stmt args = noDBerror


prepareSQL :: Connection -> String -> IO Statement
prepareSQL db sql = noDBerror


-- | Connect to a relational database
connect :: String   -- ^ database name
           -> IO Connection
connect dbname = noDBerror


-- | Disconnect from the relational database
disconnect :: Connection   -- ^ database connection
              -> IO ()
disconnect db = noDBerror


-- | commit the updates to the database
commit :: Connection   -- ^ database connection
          -> IO ()
commit db = noDBerror


-- | rollback the updates from the database
rollback :: Connection   -- ^ database connection
            -> IO ()
rollback db = noDBerror


-- | Print the relational schema of the XML document stored in the database under the given name
printSchema :: Connection   -- ^ database connection
               -> String    -- ^ schema name
               -> IO ()
printSchema db name = noDBerror


-- | Create a schema for an XML document into the database under the given name.
genSchema :: Connection   -- ^ database connection
             -> FilePath  -- ^ XML document pathname
             -> String    -- ^ schema name
             -> IO Table
genSchema db file name = noDBerror


-- | Store an XML document into the database under the given name.
shred :: Connection   -- ^ database connection
         -> FilePath  -- ^ XML document pathname
         -> String    -- ^ schema name
         -> IO ()
shred db file name = noDBerror


-- | Store an XML document into the database under the given name. Generates Haskell code.
shredC :: String      -- ^ database name
          -> FilePath -- ^ XML document pathname
          -> String   -- ^ schema name
          -> Q Exp
shredC db file name = noDBerror


-- | Create a secondary index on tagname for the shredded document under the given name..
createIndex :: Connection -- ^ database connection
               -> String  -- ^ schema name
               -> String  -- ^ the tag name of the elements to be indexed
               -> IO ()
createIndex db name tagname = noDBerror


insertDB :: Connection -> XSeq -> XSeq -> IO XSeq
insertDB db from into = noDBerror


deleteDB :: Connection -> XSeq -> IO XSeq
deleteDB db from = noDBerror


replaceDB :: Connection -> XSeq -> XSeq -> IO XSeq
replaceDB db dest with = noDBerror
