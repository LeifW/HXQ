{-------------------------------------------------------------------------------------
-
- The HDBC driver that use sqlite3
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 05/30/08, last update: 10/24/08
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}


module Connect (Connection,connectionDriver,connect) where

import Database.HDBC.Sqlite3


connectionDriver = "sqlite"


-- | Connect to the relational database in filepath using the HDBC Sqlite3 driver
connect :: FilePath -> IO Connection
connect filepath = connectSqlite3 filepath
