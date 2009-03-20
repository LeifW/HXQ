{-------------------------------------------------------------------------------------
-
- Database connectivity using HDBC
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 05/12/08, last update: 01/06/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


module Text.XML.HXQ.DB
    ( publishXmlDoc, executeSQL, prepareSQL, genSchema, shred, shredC,
      printSchema, createIndex, insertDB, deleteDB, replaceDB ) where

import System.IO.Unsafe
import Char(isSpace,isDigit,toLower)
import List(zip)
import Data.List(foldl')
import Database.HDBC
import Text.XML.HXQ.XTree
import Text.XML.HXQ.Functions(child_step)
import XMLParse(XMLEvent(..),parseDocument)
import HXML(AttList)
import Text.XML.HXQ.Parser
import Connect
import Language.Haskell.TH


sql2xml :: SqlValue -> XTree
sql2xml value =
    case value of
      SqlString s -> XText s
      SqlByteString bs -> XText (show bs)
      SqlWord32 n -> XInt (fromEnum n)
      SqlWord64 n -> XInt (fromEnum n)
      SqlInt32 n -> XText (show n)
      SqlInt64 n -> XText (show n)
      SqlInteger n -> XInt (fromEnum n)
      SqlChar c -> XText [c]
      SqlBool b -> XBool b
      SqlDouble n -> XText (show n)
      SqlRational n -> XText (show n)
      SqlEpochTime n -> XText (show n)
      SqlTimeDiff n -> XText (show n)
      SqlNull -> XNull


xml2sql :: XTree -> SqlValue
xml2sql e =
    case e of
      XText s -> SqlString s
      XInt n -> SqlInteger (toInteger n)
      XFloat n -> SqlString (show n)
      XBool n -> SqlBool n
      XElem n _ _ _ [x] -> xml2sql x
      _ -> error ("*** Cannot convert "++show e++" into sql")


perror = error "Constructed elements do not have a parent"


executeSQL :: Statement -> XSeq -> IO XSeq
executeSQL stmt args
    = do n <- handleSqlError (execute stmt (map xml2sql args))
         result <- handleSqlError (fetchAllRowsAL stmt)
         return (map (\x -> XElem "row" [] 0 perror (map (\(s,v) -> XElem (column s) [] 0 perror [sql2xml v]) x)) result)
    where column s = if elem '.' s then tail (dropWhile (/= '.') s) else s


prepareSQL :: Connection -> String -> IO Statement
prepareSQL db sql = handleSqlError (prepare db sql)


{---------------------------------------------------------------------------------------
-- Extract the structural summary and statistics from an XML file
----------------------------------------------------------------------------------------}


-- structural summary: tag   id  max#      hasText size children
data SSnode = SSnode String !Int !Int !Int !Bool   !Int [SSnode]
            deriving (Eq,Show)


insertSS :: String -> Int -> Int -> [SSnode] -> (Int,SSnode,[SSnode])
insertSS tag count size ((SSnode n i j l b len ts):s)
    | n == tag
    = (count,SSnode n i j (l+1) b (max size len) ts,s)
insertSS tag count size (x:xs)
    = let (c,t,ts) = ((insertSS tag $! count) $! size) $! xs
      in (c,t,x:ts)
insertSS tag count size []
    = (count+1,SSnode tag (count+1) 1 1 False size [],[])


insSS :: String -> Int -> Int -> [SSnode] -> (Int,[SSnode])
insSS tag count size ns
    = let (c,t,s) = ((insertSS tag $! count) $! size) $! ns
      in (c,t:s)


getSS :: [XMLEvent] -> Int -> [String] -> [SSnode] -> [SSnode]
getSS ((EmptyEvent n atts):xs) count ignored rs
    = ((getSS ((StartEvent n atts):(EndEvent n):xs) $! count) $! ignored) $! rs
getSS ((StartEvent n atts):xs) count ignored rs
    | elem n ignored
    = ((getSS xs $! count) $! ignored) $! rs
getSS ((EndEvent n):xs) count ignored rs
    | elem n ignored
    = ((getSS xs $! count) $! ignored) $! rs
getSS ((StartEvent n atts):xs) count ignored ((SSnode m i j l b len ns):rs)
    = let (c,SSnode m' i' j' l' b' len' ks,ts) = (insertSS n $! count) 0 $! ns
          (nc,as) = foldr (\(a,v) (i,s) -> ((insSS ('@':a) $! i) $! (length v)) $! s) (c,ks) atts
          r (SSnode m i j _ b len ts) = SSnode m i j 0 b len ts
          reset (SSnode m i j l b len ts) = SSnode m i j l b len (map r ts)
      in ((getSS xs $! nc) $! ignored) $! (reset (SSnode m' i' j' l' b' len' as):(SSnode m i j l b len ts):rs)
getSS ((EndEvent n):xs) count ignored (t:(SSnode m i j l b len ns):rs)
    = let s (SSnode m i j l b len ts) = SSnode m i (max j l) 0 b len ts
          set (SSnode m i j l b len ts) = SSnode m i j l b len (map s ts)
      in ((getSS xs $! count) $! ignored) $! ((SSnode m i j l b len (set t:ns):rs))
getSS ((TextEvent t):xs) count ignored ((SSnode m i j l _ len ns):rs)
    | any (not . isSpace) t
    = ((getSS xs $! count) $! ignored) $! ((SSnode m i j l True (max len (length t)) ns):rs)
getSS (_:xs) count ignored rs
    = ((getSS xs $! count) $! ignored) $! rs
getSS [] _ _ rs = rs


{---------------------------------------------------------------------------------------
-- Derive a good relational schema based on the structural summary (using hybrid inlining)
----------------------------------------------------------------------------------------}


type Path = [String]


data Table = Table String Path Bool [Table]  -- table-name relative-path mixed-content? components
           | Column String Path Int          -- column-name relative-path max-byte-size
           deriving (Show,Read)


printPath :: Path -> String
printPath [] = ""
printPath [p] = p
printPath (p:ps) = printPath ps++"/"++p


pathCons p ps = if p=="root" then ps else p:ps


schema :: SSnode -> String -> [String] -> [Table]
schema (SSnode n i _ (-1) _ len ts) prefix path
    = [ Table (prefix++"_"++show i) (pathCons n path) True
              ((reverse (concatMap (\t -> schema t prefix []) ts))
               ++[ Column "value" [] len ]) ]
schema (SSnode ('@':n) i j _ _ len []) prefix path
    = [ Column (prefix++"_"++show i) (pathCons ('@':n) path) len ]
schema (SSnode n i 1 _ _ len []) prefix path
    = [ Column (prefix++"_"++show i) (pathCons n path) len ]
schema (SSnode n i 1 _ _ _ ts) prefix path
    = concatMap (\t -> schema t prefix (n:path)) ts
schema (t@(SSnode n i _ _ b len ts)) prefix path
    = [ Table (prefix++"_"++show i) (pathCons n path) False
              ((reverse (concatMap (\t -> schema t prefix []) ts))
              ++concatMap (getContent []) ts
              ++(if b && all (\(SSnode x _ _ _ _ _ _)-> head x == '@') ts
                 then [ Column "value" [] len ] else [])) ]
      where getContent _ (SSnode _ _ _ (-1) _ _ ts) = []
            getContent ps (SSnode n i _ _ True len (_:_))
                = [Column (prefix++"_"++show i) (n:ps) len]
            getContent ps (SSnode n _ 1 _ _ _ ts)
                = concatMap (getContent (n:ps)) ts
            getContent _ _ = []


-- if an element has both text content and subelements, then it must be mixed content;
fixSS :: SSnode -> SSnode
fixSS (SSnode n i j l True len ts)
    | any (\(SSnode x _ _ _ _ _ _)-> head x /= '@') ts
    = SSnode n i j (-1) True len (map fixSS ts)
fixSS (SSnode n i j l b len ts)
    = SSnode n i j l b len (map fixSS ts)


deriveSchema :: String -> String -> [String] -> IO Table
deriveSchema file prefix ignored
    = do doc <- readFile file
         let ts = parseDocument doc
             [SSnode _ _ _ _ _ _ [t]] = getSS ts 0 ignored [SSnode "root" 1 1 1 False 0 []]
             nt@(SSnode m i j l b len s) = fixSS t
         -- putStrLn (show nt)
         return $! (head (schema (SSnode m i 2 l b len s) prefix []))


relationalSchema :: Table -> String -> [String]
relationalSchema (Table n path b ts) parent
    = ["\ncreate table "++n++" (      /* "++printPath path
       ++(if b then " (mixed content)" else "")++" */\n"
       ++n++"_id integer primary key not null"
       ++",\n"++n++"_parent integer"++(if parent /= "" then (" references "++parent++"("++parent++"_id)") else "")
       ++(concat [ ",\n"++m++" varchar("++show size++")    /* "++printPath p++" */" | Column m p size <- ts ])
       ++")\n"]
      ++ (if parent /= "" then ["create index "++n++"_parent_index on "++n++" ("++n++"_parent)\n"] else [])
      ++ [ s | t@(Table _ _ _ _) <- ts, s <- relationalSchema t n ]


getTableNames :: Table -> [String]
getTableNames (Table n _ _ ts) = n:(concatMap getTableNames ts)
getTableNames _ = []


initializeDB :: Connection -> IO ()
initializeDB db
    = do tbs <- getTables db        -- mySql always returns []
         desc <- if null tbs && connectionDriver /= "sqlite"
                 then describeTable db "HXQCatalog"     -- sqlite3 doesn't support this
                 else return []
         if null desc && not(elem "HXQCatalog" tbs)
            then do let s = "create table HXQCatalog ( name varchar(20) primary key not null,"
                            ++" next_id integer, path varchar(100),"
                            ++" summary varchar(100000), relational_schema varchar(10000) )"
                    handleSqlError (run db s [])
                    commit db
            else return ()


createSchema :: Connection -> String -> String -> [String] -> IO Table
createSchema db file name ignored
    = do initializeDB db
         stmt <- handleSqlError (prepare db "select summary from HXQCatalog where name = ?")
         _ <- handleSqlError (execute stmt  [SqlString name])
         result <- handleSqlError (fetchAllRowsAL stmt)
         if length result > 0
            then do let [[(_,SqlString s)]] = result
                        summary = (read s)::Table
                        tables = getTableNames summary
                    _ <- mapM (\t -> handleSqlError (run db ("drop table if exists "++t) []))
                              (reverse tables)
                    _ <- handleSqlError (run db "delete from HXQCatalog where name = ?" [SqlString name])
                    commit db
            else return ()
         t <- deriveSchema file name ignored
         let schema = relationalSchema t ""
         _ <- handleSqlError (run db "insert into HXQCatalog values (?,?,?,?,?)"
                                      [SqlString name, SqlInteger 0, SqlString file,
                                       SqlString (show t), SqlString (concat schema)])
         _ <- mapM (\s -> handleSqlError (run db s [])) schema
         commit db
         return $! t


-- | Create a schema for an XML document into the database under the given name.
-- The excluded tags are HTML tags to be ignored
genSchema :: Connection -> String -> String -> [String] -> IO Table
genSchema db file name excludedTags
    = createSchema db file (map toLower name) excludedTags


findSchema :: Connection -> String -> IO Table
findSchema db name
    = do initializeDB db
         stmt <- handleSqlError (prepare db "select summary from HXQCatalog where name = ?")
         _ <- handleSqlError (execute stmt  [SqlString name])
         result <- handleSqlError (fetchAllRowsAL stmt)
         if length result == 1
            then let [[(_,SqlString s)]] = result
                 in return $! ((read s)::Table)
            else error ("*** Schema "++name++" doesn't exist")


-- | Print the relational schema of the XML document stored in the database under the given name
printSchema :: Connection -> String -> IO ()
printSchema db name
    = do initializeDB db
         stmt <- handleSqlError (prepare db "select relational_schema from HXQCatalog where name = ?")
         _ <- handleSqlError (execute stmt  [SqlString name])
         result <- handleSqlError (fetchAllRowsAL stmt)
         if length result == 1
            then let [[(_,SqlString s)]] = result
                 in putStrLn s
            else error ("*** Schema "++name++" doesn't exist")


{---------------------------------------------------------------------------------------
-- Populate the database from the XML file and its derived structural summary
----------------------------------------------------------------------------------------}


findPath :: [Table] -> [String] -> Int -> Maybe (Int,Table)
findPath (t@(Table _ p _ s):ts) path _
    | p == path
    = Just (length[ 1 | Column _ _ _ <- s]-1,t)
findPath (t@(Column _ p _):ts) path n
    | p == path
    = Just (n,t)
findPath ((Table _ _ _ s):ts) path n
    = let xs = findPath s path n
          xt = findPath ts path n
      in case xs of Nothing -> xt; _ -> xs
findPath (_:ts) path n
    = findPath ts path (n+1)
findPath [] _ _ = Nothing


populate :: [XMLEvent] -> [Table] -> Int -> [[String]] -> [(Int,String)]
populate ((EmptyEvent tag atts):xs) ts n ps
    = populate ((StartEvent tag atts):(EndEvent tag):xs) ts n ps
populate (x@(StartEvent tag atts):xs) ((t@(Table n path _ s)):ts) _ (p:ps)
    = case findPath s (tag:p) 0 of
        Just (n,nt@(Table m _ _ as))
            -> (-1,m):((popAtts atts as)++(populate xs (nt:t:ts) n ([]:p:ps)))
        Just (n,nt)
            -> map (\(a,v) -> case findPath ts (('@':a):tag:p) 0 of
                                Just (n,_) -> (n,v)
                                Nothing -> error ("*** Unrecognized attribute: "++a)) atts
               ++ populate xs (nt:t:ts) n ((tag:p):ps)
        Nothing -> populate xs (t:ts) 0 ((tag:p):ps)
      where popAtts ((a,v):as) ks
                = let Just(m,_) = findPath ks ['@':a] 0
                  in (m,v):(popAtts as ks)
            popAtts [] _ = []
populate ((EndEvent tag):xs) ((t@(Table n path _ s)):ts) _ ([]:ps)
    = (-2,n):populate xs ts 0 ps
populate ((EndEvent tag):xs) ((Column m path _):ts) n (p:ps)
    = populate xs ts 0 (tail p:ps)
populate ((EndEvent text):xs) ts _ (p:ps)
    = populate xs ts 0 (tail p:ps)
populate ((TextEvent text):xs) ts n ps
    | any (not . isSpace) text
    = (n,text):populate xs ts n ps
populate (x:xs) ts n ps
    = populate xs ts n ps
populate [] ts n ps = []


insert :: [(Int,String)] -> Integer -> Integer -> [(String,Int,Statement)] -> IO (Integer,[(Int,String)])
insert xs id parent stmts = let (s,binds,rest,_) = m xs id parent
                            in if null rest
                               then do n <- s; return (n,binds)
                               else do n <- s
                                       (m,mb) <- insert rest n parent stmts
                                       return (m,mb++binds)
    where m ((-1,m):xs) i p = let (s,el,xs',i') = ml xs (i+1) i
                              in ((insertTuple m el i p) >> s,[],xs',i')
          m ((k,m):xs) i p = (return i,[(k,m)],xs,i)
          m [] i p = (return i,[],[],i)
          ml [] i p = (return i,[],[],i)
          ml ((-2,m):xs) i p = (return i,[],xs,i)
          ml xs i p = let (s,el,xs',i') = m xs i p
                          (s',el',xs'',i'') = ml xs' i' p
                      in (s >> s',el++el',xs'',i'')
          find x xs = foldl' (\r (a,v) -> if x==a then v else r) "\NUL" xs
          insertTuple m e i p
              = let (len,stmt) = foldr (\(a,l,s) r -> if m==a then (l,s) else r) (error "*** sql stmt not found") stmts
                    tuple = map (\c -> find c e) [0..len]
                    lift x = if x=="\NUL" then SqlNull else SqlString x
                in do -- putStrLn (m++show(show i:show p:tuple))
                      _ <- handleSqlError (execute stmt (SqlInteger i:SqlInteger p:(map lift tuple)))
                      return i


tableStmt db root (Table n _ _ ts)
    = do let len = length[ 1 | Column _ _ _ <- ts ]-1
         stmt <- handleSqlError (prepare db ("insert into "++n++" values (?,?"
                                             ++(concatMap (\_ -> ",?") [0..len])++")"))
         l <- mapM (tableStmt db root) ts
         return $! ((n,len,stmt):(concat l))
tableStmt _ _ _ = return []


-- | Store an XML document into the database under the given name.
shred :: Connection -> String -> String -> IO ()
shred db file name
    = do let prefix = map toLower name
         t@(Table root _ _ _) <- findSchema db prefix
         stmts <- tableStmt db root t
         stmt1 <- prepare db "select next_id from HXQCatalog where name = ?"
         _ <- execute stmt1 [SqlString prefix]
         [[(_,SqlString ids)]] <- fetchAllRowsAL stmt1
         doc <- readFile file
         let id = (read ids)+1
             ts = parseDocument doc
             ic = populate ts [Table prefix [] True [t]] 0 [[]]
         -- putStrLn (show ic)
         (new_id,_) <- insert ic id 0 stmts
         stmt2 <- prepare db "update HXQCatalog set next_id = ? where name = ?"
         execute stmt2 [SqlInteger new_id,SqlString prefix]
         commit db
         return ()


-- | Create a secondary index on tagname for the shredded document under the given name..
createIndex :: Connection -> String -> String -> IO ()
createIndex db name tagname
    = do let prefix = map toLower name
         table <- findSchema db name
         let indexes = getIndexes "" table
         _ <- if null indexes
              then error ("*** Tthere is no tagname: "++tagname)
              else mapM (\(t,c) -> do -- putStrLn (t++" "++c)
                                      stmt <- handleSqlError (prepare db ("create index "++t++"_"++c++" on "++t++" ("++c++")"))
                                      handleSqlError (execute stmt [])) indexes
         commit db
         return ()
    where getIndexes _ (Table n (p:_) _ _) | p==tagname = [(n,"value")]
          getIndexes _ (Table n _ _ ts) = concatMap (getIndexes n) ts
          getIndexes table (Column n (p:_) _) | p==tagname = [(table,n)]
          getIndexes _ _ = []


{---------------------------------------------------------------------------------------
-- Generate Haskell code to populate the database from an XML file
----------------------------------------------------------------------------------------}


{-# NOINLINE insertTuple #-}
insertTuple :: Connection -> Statement -> String -> Int
            -> [Integer] -> [[(Int,String)]] -> [[(Int,String)]]
insertTuple db stmt nm len (id:parent:_) (c:cs)
         = let find x xs = foldr (\(a,v) r -> if x==a then v else r) "\NUL" xs
               tuple = map (\a -> find a c) [2..len]
               lift x = if x=="\NUL" then SqlNull else SqlString x
               i = toInteger id
               p = toInteger parent
               query = unsafePerformIO
                       (do -- putStrLn (nm++show(show i:show p:tuple))
                           catchSql (do execute stmt (SqlInteger i:SqlInteger p:(map lift tuple))
                                        return ())
                                    (\ e -> putStrLn (show e++show cs))
                           if mod id 10000 == 9999
                              then do putStrLn (show (id+1)++" tuples")
                                      commit db
                              else return ())
           in query `seq` cs


pushColumn :: (Int,String) -> [[(Int,String)]] -> [[(Int,String)]]
pushColumn a (x:xs) = (a:x):xs


pushAttributes :: [(String,String)] -> [(Int,String)] -> [[(Int,String)]] -> [[(Int,String)]]
pushAttributes atts ps (x:xs)
    = ((map (\(a,v) -> findColumn a v ps) atts)++x):xs
      where findColumn name value ps
                = foldr (\(i,a) r -> if a==name then (i,value) else r)
                        (error ("*** Column "++name++" not found")) ps


dfa state stream values i ancs c
    = let (n,vs,ni,nancs,s) = ((c (state,stream) $! i) $! ancs) $! values
      in if n == 0
         then i+1
         else dfa n s vs ni nancs c


{-# NOINLINE shredC #-}
-- | Store an XML document into the database under the given name. Generates Haskell code.
shredC :: String -> String -> String -> Q Exp
shredC dbname file name
  = unsafePerformIO (
      do let prefix = map toLower name
             rev (Table n path b ts) = Table n (reverse path) b (map rev ts)
             rev (Column tag path len) = Column tag (reverse path) len
         dbc <- connect dbname
         tbl <- findSchema dbc prefix
         let table = rev tbl
             intE = litE . integerL . toInteger
             intP = litP . integerL . toInteger
             pathHead (Table _ (tag:_:_) _ _) = Just tag
             pathHead (Column _ (tag:_:_) _) = Just tag
             pathHead _ = Nothing
             filter _ [] = ([],[])
             filter tag ((Table n (p:ps) b s):ts)
                 | p == tag
                 = let (s1,s2) = filter tag ts
                   in ((Table n ps b s):s1,s2)
             filter tag ((Column n (p:ps) len):ts)
                 | p == tag
                 = let (s1,s2) = filter tag ts
                   in ((Column n ps len):s1,s2)
             filter tag (t:ts)
                 = let (s1,s2) = filter tag ts
                   in (s1,t:s2)
             findTag tag = foldr (\(n,t) r -> if t==tag then n else r) (error "tag not found")
             getColumns nm ts = zip [2..] [ a | Column a _ _ <- ts ]
             getAttributes nm ts = [ (i,a) | (i,['@':a]) <- zip [2..] [ p | Column _ p _ <- ts ] ]
             genCodeL [] state avail _
                 = (avail,[])
             genCodeL (x@(t:ts)) state avail cols
                 = case pathHead t of
                     Nothing
                         -> let (i1,c1) = genCode t state avail cols
                                (i2,c2) = genCodeL ts state i1 cols
                            in (i2,c1++c2)
                     Just tag -> let (s1,s2) = filter tag x
                                     (i1,c1) = genCodeL s1 avail (avail+1) cols
                                     (i2,c2) = if null s2 then (i1,[]) else genCodeL s2 state i1 cols
                                     atts = [ (findTag m cols,a) | Column m ['@':a] _ <- s1++s2  ]
                                 in (i2,skipTag tag state avail atts ++ c1 ++ c2)
             skipTag tag state avail atts
                 = let next = avail
                       ae xs = if null atts
                               then [| $xs |]
                               else [| pushAttributes $al $(listE (map (\(i,a) -> tupE [intE i,litE (stringL a)]) atts)) $xs |]
                   in [ match (tupP [intP state,infixP (conP (mkName "StartEvent") [litP (stringL tag),alp]) (mkName ":") rp])
                              (normalB [| ($(intE next),$(ae cs),$current,$ancestors,$r) |]) [],
                        match (tupP [intP next,infixP (conP (mkName "EndEvent") [litP (stringL tag)]) (mkName ":") rp])
                              (normalB [| ($(intE state),$cs,$current,$ancestors,$r) |]) [],
                        match (tupP [intP state,infixP (conP (mkName "EmptyEvent") [litP (stringL tag),alp]) (mkName ":") rp])
                              (normalB [| ($(intE state),$(ae cs),$current,$ancestors,$r) |]) []]
             genCode (Column "value" [] _) state avail cols
                 = (avail,[ match (tupP [intP state,infixP (conP (mkName "TextEvent") [dp]) (mkName ":") rp])
                                  (normalB [| ($(intE state),pushColumn ($(intE (findTag "value" cols)),$d) $cs,
                                               $current,$ancestors,$r) |]) [] ])
             genCode (Column nm ['@':tag] _) state avail _
                 = (avail,[])
             genCode (Column nm [] _) state avail cols
                 = (avail,[ match (tupP [intP state,infixP (conP (mkName "TextEvent") [dp]) (mkName ":") rp])
                                  (normalB [| ($(intE state),pushColumn ($(intE (findTag nm cols)),$d) $cs,
                                               $current,$ancestors,$r) |]) [] ])
             genCode (Table nm [tag] mixed ts) state avail _
                 = let next = avail
                       ncs = [| [] : $cs |]
                       cols = getColumns nm ts
                       atts = getAttributes nm ts
                       ae xs = if null atts
                               then [| $xs |]
                               else [| pushAttributes $al $(listE (map (\(i,a) -> tupE [intE i,litE (stringL a)]) atts)) $xs |]
                       (i,c) = genCodeL ts next (avail+1) cols
                   in (i,[ match (tupP [intP state,infixP (conP (mkName "StartEvent") [litP (stringL tag),alp]) (mkName ":") rp])
                                         (normalB [| ($(intE next),$(ae ncs),$current+1,($current+1) : $ancestors,$r) |]) [],
                           match (tupP [intP next,infixP (conP (mkName "EndEvent") [litP (stringL tag)]) (mkName ":") rp])
                                         (normalB [| ($(intE state),
                                                       insertTuple $db $(varE (mkName (nm++"_stmt"))) $(litE (stringL nm))
                                                                       $(intE ((length cols)+1)) $ancestors $cs,
                                                      $current,tail $ancestors,$r) |]) [],
                           match (tupP [intP state,infixP (conP (mkName "EmptyEvent") [litP (stringL tag),alp]) (mkName ":") rp])
                                         (normalB [| ($(intE state),
                                                       insertTuple $db $(varE (mkName (nm++"_stmt"))) $(litE (stringL nm))
                                                                       $(intE ((length cols)+1)) (($current+1) : $ancestors) $(ae ncs),
                                                      $current+1,$ancestors,$r) |]) [] ]
                           ++ c)
             genCode (Column nm (tag:path) k) state avail cols
                 = let (i,s) = genCode (Column nm path k) avail (avail+1) cols
                   in (i,skipTag tag state avail []++s)
             genCode (Table nm (tag:path) mixed ts) state avail cols
                 = let (i,s) = genCode (Table nm path mixed ts) avail (avail+1) cols
                   in (i,skipTag tag state avail []++s)
             s = varE (mkName "s")
             r = varE (mkName "r")
             current = varE (mkName "i")
             ancestors = varE (mkName "ancs")
             d = varE (mkName "d")
             id = varE (mkName "id")
             al = varE (mkName "al")
             cs = varE (mkName "cs")
             db = varE (mkName "db")
             sp = varP (mkName "s")
             rp = varP (mkName "r")
             dp = varP (mkName "d")
             alp = varP (mkName "al")
             (_,caseCode) = genCode table 1 2 []
             code = lamE [sp,varP (mkName "i"),varP (mkName "ancs"),varP (mkName "cs")]
                    (caseE (varE (mkName "s"))
                           (caseCode++[match (tupP [sp,infixP wildP (mkName ":") rp])
                                             (normalB [| ($s,$cs,$current,$ancestors,$r) |]) [],
                                       match wildP (normalB [| (0,[],1,[],[]) |]) []]))
             tableStmt (Table n _ _ ts)
                 = let len = length[ 1 | Column _ _ _ <- ts ]-1
                       ins = "insert into "++n++" values (?,?"
                             ++(concatMap (\_ -> ",?") [0..len])++")"
                       stmt = [| prepare $db $(litE (stringL ins)) |]
                   in (n++"_stmt",stmt):concatMap tableStmt ts
             tableStmt _ = []
             mseq a v b = infixE (Just a) (varE (mkName ">>=")) (Just (lamE [varP (mkName v)] b))
             ret = foldr (\(n,s) r -> mseq s n r)
                         [| return $! dfa 1 $(varE (mkName "doc")) [[]] $id [0] $code |]
                         (tableStmt table)
         -- runQ code >>= putStrLn.pprint
         return $! [| do d <- readFile $(litE (stringL file))
                         let doc = parseDocument d
                         db <- connect $(litE (stringL dbname))
                         stmt <- prepare db "select next_id from HXQCatalog where name = ?"
                         _ <- handleSqlError (execute stmt [SqlString prefix])
                         [[(_,SqlString ids)]] <- fetchAllRowsAL stmt
                         let id = read ids
                         new_id <- handleSqlError ($(lamE [varP (mkName "db"),varP (mkName "doc"),varP (mkName "id")] ret)
                                                    db doc id)
                         stmt2 <- prepare db "update HXQCatalog set next_id = ? where name = ?"
                         handleSqlError (execute stmt2 [SqlInteger new_id,SqlString prefix])
                         commit db
                   |])


{----------------------------------------------------------------------------------------------------
--  Export (publish) a shredded XML document
----------------------------------------------------------------------------------------------------}


-- construct an XQuery (in string form) that extracts a shredded XML document
publishTable :: Table -> String -> Bool -> String
publishTable (table@(Table n _ _ _)) schema needsParent
    = "<root>{attribute {'_id'} {'0'},attribute {'_parent'} {()}"
      ++",attribute {'_table'} {'"++schema++" "++n++" "++n++"'}}"
      ++ pubS table "()" id ++ "</root>"
      where pubS (Table n (p:_) _ ts) parent c
                = c ("{for $"++n++" in SQL(select(),tables($"++n++"),"
                     ++(if parent == "()"
                        then "true()"
                        else "$"++n++"/"++n++"_parent eq $"++parent++"/"++parent++"_id")
                     ++") return <"++p++">"++header n n False++"}"++ pubLS ts n parent id ++ "</"++p++">}")
            pubS (Column tag (('@':p):_) _) parent c
                = c ("{attribute "++p++" {$"++parent++"/"++tag++"/text()}}")
            pubS (Column tag (p:_) _) parent c
                = c ("<"++p++">"++header tag parent False++",$"++parent++"/"++tag++"/text()}</"++p++">")
            pubS (Column tag [] _) parent c
                = c ("{$"++parent++"/"++tag++"/text()}")
            header tag parent composite
                = "{attribute {'_id'} {$"++parent++"/"++parent++"_id/text()}"
                  ++",attribute {'_parent'} {"++(if needsParent && parent /= "()/()_id" then "$"++parent else "()")++"}"
                  ++",attribute {'_table'} {'"++schema++" "++parent++(if composite then " +" else " ")++tag++"'}"
            pubLS [] _ _ c = c ""
            pubLS (x@(t:ts)) n parent c
                = case head t of
                    Nothing -> (pubS t n c)++(pubLS ts n parent c)
                    Just tag -> let (s1,s2) = filter tag (reverse x)
                                in (mkE tag s1 c)++(if null s2 then "" else pubLS s2 n parent id)
                  where mkE tag s c
                            = "<"++tag++">"++header tag n True++"}"++pubLS s n parent c++"</"++tag++">"
                        head (Table _ (p:_:_) _ _) = Just p
                        head (Column _ (p:_:_) _) = Just p
                        head _ = Nothing
                        filter _ [] = ([],[])
                        filter tag ((Table n (p:ps) b s):ts)
                            | p == tag
                            = let (s1,s2) = filter tag ts
                              in ((Table n ps b s):s1,s2)
                        filter tag ((Column n (p:ps) len):ts)
                            | p == tag
                            = let (s1,s2) = filter tag ts
                              in ((Column n ps len):s1,s2)
                        filter tag (t:ts)
                            = let (s1,s2) = filter tag ts
                              in (s1,t:s2)


{-# NOINLINE publishXmlDoc #-}
-- construct the Ast of an XQuery that extracts a shredded  XML document
publishXmlDoc :: FilePath -> String -> Bool -> Ast
publishXmlDoc filepath name needsParent
    = let query = unsafePerformIO (publishWrapper filepath name)
          [ast] = parse (scan query)
      in ast
    where rev (Table n path b ts) = Table n (reverse path) b (map rev ts)
          rev (Column tag path len) = Column tag (reverse path) len
          publishWrapper filepath name
              = do let prefix = map toLower name
                   db <- connect filepath
                   table <- findSchema db prefix
                   -- putStrLn (show table)
                   let query = publishTable (rev table) prefix needsParent
                   -- putStrLn query
                   return $! query


{----------------------------------------------------------------------------------------------------
--  Database updates
----------------------------------------------------------------------------------------------------}


dbError :: Connection -> String -> IO a
dbError db msg = rollback db >> error ("*** "++msg)


toStream :: XSeq -> [XMLEvent]
toStream ((XElem tag al _ _ ts):xs)
    = (StartEvent tag al):(toStream ts++((EndEvent tag):toStream xs))
toStream (x:xs) = (TextEvent (show x)):toStream xs
toStream [] = []


insertChildren :: Connection -> String -> Table -> XSeq -> Int -> IO [(Int,String)]
insertChildren db schema table children parent
    = do stmts <- tableStmt db schema table
         stmt1 <- prepare db "select next_id from HXQCatalog where name = ?"
         execute stmt1 [SqlString schema]
         [[(_,SqlString ids)]] <- fetchAllRowsAL stmt1
         let id = (read ids)+1
             Table tag _ _ ts = table
             ic = populate (toStream children) [table,table] 0 [[],[]]
         (new_id,binds) <- insert ic id (toInteger parent) stmts
         stmt2 <- prepare db "update HXQCatalog set next_id = ? where name = ?"
         execute stmt2 [SqlInteger new_id,SqlString schema]
         return binds


getDestinationTable :: Table -> String -> [Table]
getDestinationTable (x@(Table t _ _ ts)) tname
    | t == tname
    = [x]
getDestinationTable (Table t _ _ ts) tname
    = concatMap (\t -> getDestinationTable t tname) ts
getDestinationTable _ _ = []


insertDB :: Connection -> XSeq -> XSeq -> IO XSeq
insertDB db from into
    = case into of
        [d@(XElem tag (("_table",tnm):_) id parent cs)]
            -> do let [schema,tableName,attrName] = words tnm
                  table <- findSchema db schema
                  let [dest@(Table _ _ _ tbs)] = getDestinationTable table tableName
                  if attrName == tableName
                     then insertChildren db schema dest from id
                     else do mapM (\x -> case x of
                                           XElem t _ _ _ ts
                                               -> if head attrName == '+'
                                                  then case [ z | z@(Table _ (tn:_) _ _) <- tbs, t==tn ] of
                                                         [Table tx (tn:_) tb s]
                                                             -> insertChildren db schema (Table tableName [] False [Table tx [tn] tb s]) from id
                                                         _ -> case child_step t d of
                                                                c@[XElem _ _ _ _ [XNull]]
                                                                    -> replaceDB db c ts >> return []
                                                                [] -> dbError db ("You cannot insert this element at this position: "++show x)
                                                                _ -> dbError db ("There is already an inserted element "
                                                                                 ++"at this position; use replace instead: "++show x)
                                                  else case child_step t d of
                                                         c@[XElem _ _ _ _ [XNull]]
                                                             -> replaceDB db c ts >> return []
                                                         [] -> dbError db ("You cannot insert this element at this position: "++show x)
                                                         _ -> dbError db ("There is already an inserted element "
                                                                          ++"at this position; use replace instead: "++show x)
                                           _ -> dbError db ("Incompatible insertion source: "++show x)
                                  ) from
                             return []
                  return []
        _ -> dbError db ("The insert destination must be a single persistent XML element: "++show into)


removeTuples :: Connection -> String -> Table -> Integer -> IO ()
removeTuples db schema (Table n _ _ ts) parent
    = do stmt <- handleSqlError (prepare db ("select "++n++"_id from "++n++" where "++n++"_parent = ?"))
         handleSqlError (execute stmt [SqlInteger parent])
         result <- fetchAllRowsAL stmt
         case result of
           [[(_,SqlString ids)]]
               -> do let id = read ids
                     handleSqlError (run db ("delete from "++n++" where "++n++"_id = ?") [SqlInteger id])
                     mapM (\t -> removeTuples db schema t id) ts
                     return ()
           _ -> return ()
removeTuples _ _ _ _  = return ()


getColumns (XElem tag (("_table",tnm):_) id parent cs)
    = let [schema,tableName,attrName] = words tnm
      in if head attrName == '+'
         then concatMap getColumns cs
         else [attrName]
getColumns _ = []


deleteDB :: Connection -> XSeq -> IO XSeq
deleteDB db (x@(XElem tag (("_table",tnm):_) id parent cs):xs)
    = do let [schema,tableName,attrName] = words tnm
         if tableName /= attrName
            then mapM (\c -> handleSqlError (run db ("update "++tableName++" set "++c
                                                     ++" = NULL where "++tableName++"_id = ?")
                                                    [SqlInteger (toInteger id)]))
                      (getColumns x)
            else do table <- findSchema db schema
                    let [Table n _ _ ts] = getDestinationTable table tableName
                    handleSqlError (run db ("delete from "++n++" where "++n++"_id = ?")
                                           [SqlInteger (toInteger id)])
                    mapM (\t -> removeTuples db schema t (toInteger id)) ts
                    return [0]
         deleteDB db xs
deleteDB db (x:_) = dbError db ("You may only delete persistent XML elements: "++show x)
deleteDB _ [] = return []


replaceDB :: Connection -> XSeq -> XSeq -> IO XSeq
replaceDB db dest with
    = case dest of
        [d@(XElem tag (("_table",tnm):_) id parent cs)]
            -> do let [schema,tableName,attrName] = words tnm
                      update x = do handleSqlError (run db ("update "++tableName++" set "++attrName
                                                            ++" = ? where "++tableName++"_id = ?")
                                                    [SqlString (show x),SqlInteger (toInteger id)])
                                    return []
                  table <- findSchema db schema
                  if tableName == attrName
                     then let [dest@(Table _ _ _ ts)] = getDestinationTable table tableName
                          in do handleSqlError (run db ("delete from "++tableName++" where "++tableName++"_id = ?")
                                                [SqlInteger (toInteger id)])
                                mapM (\t -> removeTuples db schema t (toInteger id)) ts
                                insertChildren db schema dest with id
                                return []
                     else case with of
                            [XElem t _ _ _ ts]
                                -> if t == tag
                                   then mapM (\w -> case w of
                                                      XNoPad -> return []
                                                      XElem t' _ _ _ _ 
                                                          -> case child_step t' d of
                                                               [z] -> replaceDB db [z] [w]
                                                               _ -> dbError db ("The replace destination element tagged '"
                                                                                ++tag++"' does not have a child tagged '"++t'++"'")
                                                      _ -> if head attrName == '+'
                                                           then dbError db ("The destination element tagged '"
                                                                            ++tag++"' can only be replaced with another element")
                                                           else update w
                                             ) ts
                                   else dbError db ("The destination element tagged '"
                                                    ++tag++"' cannot be replaced with an element tagged '"++t++"'")
                            [x] | head attrName /= '+' -> update x
                            [] -> return []
                            _ -> dbError db ("The replace source must be a singleton value: "++show with)
                  return []
        _ -> dbError db ("The replace destination must be a single persistent XML element: "++show dest)
