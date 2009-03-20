{-------------------------------------------------------------------------------------
-
- Database connectivity using HDBC
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 08/14/08, last update: 08/14/08
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}


module Text.XML.HXQ.OptionalDB
    ( IConnection, Statement, Connection, publishXmlDoc, executeSQL, prepareSQL, connect, disconnect, commit, rollback,
      genSchema, shred, shredC, printSchema, createIndex, insertDB, deleteDB, replaceDB
    ) where


import Database.HDBC
import Text.XML.HXQ.DB
import Connect
