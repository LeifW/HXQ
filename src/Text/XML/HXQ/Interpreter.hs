{-------------------------------------------------------------------------------------
-
- The XQuery Interpreter
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


module Text.XML.HXQ.Interpreter
    ( xquery, xqueryDB, xfileDB, evalInput, xqueryE ) where

import Text.XML.HXQ.Parser
import Text.XML.HXQ.XTree
import Text.XML.HXQ.OptionalDB
import Control.Monad
import List(sortBy)
import XMLParse(parseDocument)
import Readline
import Text.XML.HXQ.Optimizer
import Text.XML.HXQ.Functions
import Text.XML.HXQ.Compiler
import Text.XML.HXQ.Types



-- run-time bindings of FLOWR variables
type Environment = [(String,XSeq)]


-- a user-defined function is (fname,parameters,body)
type Functions = [(String,[String],Ast)]


undefv1 = error "Undefined XQuery context (.)"
undefv2 = error "Undefined position()"
undefv3 = error "Undefined last()"


-- Each XPath predicate must calculate position() and last() from its input XSeq
-- if last() is used, then the evaluation is blocking (need to store the whole input XSeq)
applyPredicates :: [Ast] -> XSeq -> Bool -> Environment -> Functions -> XSeq
applyPredicates [] xs _ _ _ = xs
applyPredicates ((Aint n):preds) xs _ env fncs   -- shortcut that improves laziness
    = applyPredicates preds (index xs (n-1)) True env fncs
applyPredicates ((Ast "call" [Avar "last"]):preds) xs _  env fncs
    = applyPredicates preds [ last $xs ] True  env fncs
applyPredicates (pred:preds) xs True env fncs    -- top-k like
    | maxPosition pathPosition pred > 0
    = applyPredicates (pred:preds) (take (maxPosition pathPosition pred) xs) False env fncs
applyPredicates (pred:preds) xs _ env fncs
    | containsLast pred         -- blocking: use only when last() is used in the predicate
    = let last = length xs
      in applyPredicates preds
             (foldir (\x i r -> case eval pred x i last "" env fncs of
                                  [XInt k] -> if k == i then x:r else r               -- indexing
                                  b -> if conditionTest b then x:r else r) [] xs 1) True env fncs
applyPredicates (pred:preds) xs _ env fncs
    = applyPredicates preds
          (foldir (\x i r -> case eval pred x i undefv3 "" env fncs of
                               [XInt k] -> if k == i then x:r else r               -- indexing
                               b -> if conditionTest b then x:r else r) [] xs 1) True env fncs


-- The XQuery interpreter
-- context: context node (XPath .)
-- position: the element position in the parent sequence (XPath position())
-- last: the length of the parent sequence (XPath last())
-- effective_axis: the XPath axis in /axis::tag(exp)
--        eg, the effective axis of //(A | B) is descendant
-- env: contains FLOWR variable bindings
-- fncs: user-defined functions
eval :: Ast -> XTree -> Int -> Int -> String -> Environment -> Functions -> XSeq
eval e context position last effective_axis env fncs
  = case e of
      Avar "." -> [ context ]
      Avar v -> findV v env
      Aint n -> [ XInt n ]
      Afloat n -> [ XFloat n ]
      Astring s -> [ XText s ]
      Ast "context" [v,Astring dp,body]
          -> foldr (\x r -> (eval body x position last dp env fncs)++r)
                   [] (eval v context position last effective_axis env fncs)
      Ast "call" [Avar "position"] -> [XInt position]
      Ast "call" [Avar "last"] -> [XInt last]
      Ast "step" (Avar "child":tag:Avar ".":preds)
          | effective_axis /= ""
          -> eval (Ast "step" (Avar effective_axis:tag:Avar ".":preds)) context position last "" env fncs
      Ast "step" (Avar "descendant_any":Ast "tags" tags:e:preds)
          -> let ts = map (\(Avar tag) -> tag) tags
                 v = eval e context position last effective_axis env fncs
             in if v==[XNull]
                then v
                else foldr (\x r -> (applyPredicates preds (descendant_any_with_tagged_children ts x) True env fncs)++r) [] v
      Ast "step" (Avar step:Astring tag:e:preds)
          -> let step_fnc = findV step pathFunctions
                 v = eval e context position last effective_axis env fncs
             in if v==[XNull]
                then v
                else foldr (\x r -> (applyPredicates preds (step_fnc tag x) True env fncs)++r) [] v
      Ast "filter" (e:preds)
          -> applyPredicates preds (eval e context position last effective_axis env fncs) True env fncs
      Ast "predicate" [condition,body]
          -> if conditionTest (eval condition undefv1 undefv2 undefv3 "" env fncs)
             then eval body context position last effective_axis env fncs
             else []
      Ast "append" args
          -> appendText (map (\x -> eval x context position last effective_axis env fncs) args)
      Ast "if" [c,t,e]
          -> if conditionTest (eval c context position last effective_axis env fncs)
             then eval t context position last effective_axis env fncs
             else eval e context position last effective_axis env fncs
      Ast f _
          | elem f ["insert","delete","replace"]
          -> error "Updates must be over XML data stored in databases"
      Ast "call" (v@(Avar fname):args)
          -> let vs = map (\x -> eval x context position last effective_axis env fncs) args
             in case filter (\(n,_,_,_) -> n == fname || ("fn:"++n) == fname) systemFunctions of
                  [] -> if isBuildInType fname  && length vs == 1
                        then castAs (head vs) v
                        else error "External function calls must be within the IO monad"
                  fs -> case filter (\(_,len,_,_) -> len < 0 || length args == len) fs of
                          [] -> error ("wrong number of arguments in function call: " ++ fname)
                          (_,_,f,_):_ -> f vs
      Ast "construction" [tag,id,parent,Ast "attributes" al,body]
             -> let ct = eval tag context position last effective_axis env fncs
                    bc = eval body context position last effective_axis env fncs
                    (as,bs) = span (\x -> case x of XAttr _ _ -> True; _ -> False) bc
                    alc = concatMap (\(Ast "pair" [a,v])
                                     -> let ac = eval a context position last effective_axis env fncs
                                            vc = eval v context position last effective_axis env fncs
                                        in if vc==[XNull] then [] else [(qName ac,showXS vc)]) al
                          ++ [ (n,v) | XAttr n v <- as ]
                    [XText vid] = eval id context position last effective_axis env fncs
                    vparent = eval parent context position last effective_axis env fncs
                in [ XElem (qName ct) alc (read vid) (if null vparent then parent_error else head vparent) bs ]
      Ast "attribute_construction" [name,value]
          -> let ns = eval name context position last effective_axis env fncs
                 vs = eval value context position last effective_axis env fncs
             in [ XAttr (qName ns) (showXS vs) ]
      Ast "let" [Avar var,source,body]
          -> eval body context position last effective_axis
                  ((var,eval source context position last effective_axis env fncs):env) fncs
      Ast "for" [Avar var,Avar "$",source,body]      -- a for-loop without an index
          -> foldr (\a r -> (eval body a undefv2 undefv3 "" ((var,[a]):env) fncs)++r)
                   [] (eval source context position last effective_axis env fncs)
      Ast "for" [Avar var,Avar ivar,source,body]     -- a for-loop with an index
          -> let p = maxPosition (Avar ivar) body
                 ns = if p > 0              -- there is a top-k like restriction
                      then Ast "step" [source,Ast "call" [Avar "<=",pathPosition,Aint p]]
                      else source 
             in foldir (\a i r -> (eval body a i undefv3 "" ((var,[a]):(ivar,[XInt i]):env) fncs)++r)
                       [] (eval ns context position last effective_axis env fncs) 1
      Ast "sortTuple" (exp:orderBys)             -- prepare each FLWOR tuple for sorting
          -> let ee = eval exp context position last effective_axis env fncs
             in [ XElem "" [] 0 parent_error
                     (foldl (\r a -> r++[XElem "" [] 0 parent_error (text (eval a context position last effective_axis env fncs))])
                            [XElem "" [] 0 parent_error ee] orderBys) ]
      Ast "sort" (exp:ordList)                   -- blocking
          -> let ce = map (\(XElem _ _ _ _ xs) -> map (\(XElem _ _ _ _ ys) -> ys) xs)
                          (eval exp context position last effective_axis env fncs)
                 ordering = foldr (\(Avar ord) r (x:xs) (y:ys)
                                       -> case compareXSeqs (ord == "ascending") x y of
                                            EQ -> r xs ys
                                            o -> o)
                                  (\xs ys -> EQ) ordList
             in concatMap head (sortBy (\(_:xs) (_:ys) -> ordering xs ys) ce)
      Ast "type" [tp]
          -> [ XType tp ]
      _ -> error ("Illegal XQuery: "++(show e))


type Statements = [(String,Statement)]


-- The monadic applyPredicates that propagates IO state
applyPredicatesM :: [Ast] -> XSeq -> Bool -> Environment -> Functions -> Connection -> Statements -> IO XSeq
applyPredicatesM [] xs _ _ _ _ _ = return $! xs
applyPredicatesM ((Aint n):preds) xs _ env fncs db stmts   -- shortcut that improves laziness
    = applyPredicatesM preds (index xs (n-1)) True env fncs db stmts
applyPredicatesM (pred:preds) xs True env fncs db stmts    -- top-k like
    | maxPosition pathPosition pred > 0
    = applyPredicatesM (pred:preds) (take (maxPosition pathPosition pred) xs) False env fncs db stmts
applyPredicatesM ((Ast "call" [Avar "last"]):preds) xs _  env fncs db stmts
    = applyPredicatesM preds [ last $xs ] True  env fncs db stmts
applyPredicatesM (pred:preds) xs _ env fncs db stmts
    | containsLast pred         -- blocking: use only when last() is used in the predicate
    = do let last = length xs
         vs <- foldir (\x i r -> do vs <- evalM pred x i last "" env fncs db stmts
                                    s <- r
                                    return $! (if case vs of
                                                    [XInt k] -> k == i               -- indexing
                                                    b -> conditionTest b
                                               then x:s else s))
                      (return []) xs 1
         applyPredicatesM preds vs True env fncs db stmts
applyPredicatesM (pred:preds) xs _ env fncs db stmts
    = do vs <- foldir (\x i r -> do vs <- evalM pred x i undefv3 "" env fncs db stmts
                                    s <- r
                                    return $! (if case vs of
                                                    [XInt k] -> k == i               -- indexing
                                                    b -> conditionTest b
                                               then x:s else s))
                      (return []) xs 1
         applyPredicatesM preds vs True env fncs db stmts


-- The monadic XQuery interpreter; it is like eval but has plumbing to propagate IO state
evalM :: Ast -> XTree -> Int -> Int -> String -> Environment -> Functions -> Connection -> Statements -> IO XSeq
evalM e context position last effective_axis env fncs db stmts
  = case e of
      Avar "." -> return $! [ context ]
      Avar v -> return $! (findV v env)
      Aint n -> return $! [ XInt n ]
      Afloat n -> return $! [ XFloat n ]
      Astring s -> return $! [ XText s ]
      -- for non-IO XQuery, use the regular eval
      Ast "nonIO" [u] -> return $! (eval u context position last effective_axis env fncs)
      Ast "context" [v,Astring dp,body]
          -> do vs <- evalM v context position last effective_axis env fncs db stmts
                foldr (\x r -> (liftM2 (++)) (evalM body x position last dp env fncs db stmts) r)
                      (return []) vs
      Ast "call" [Avar "position"] -> return $! [XInt position]
      Ast "call" [Avar "last"] -> return $! [XInt last]
      Ast "call" [Avar f,Astring file]
          | elem f ["doc","fn:doc"]
          -> do doc <- uploadFile file
                return $! [materialize False (parseDocument doc)]
      Ast "call" [Avar "debug",c]
          -> do ec <- evalM c context position last effective_axis env fncs db stmts
                debugSession ec env fncs db
      Ast "call" [Avar "eval",x]
          -> do xc <- evalM x context position last effective_axis env fncs db stmts
                case xc of
                  [ XText q ] -> do (res,_,_) <- evalQueryM (parse (scan q)) env fncs db False
                                    return res
                  _ -> error $ "The eval argument must be a string: " ++ show xc
      Ast "step" (Avar "child":tag:Avar ".":preds)
          | effective_axis /= ""
          -> evalM (Ast "step" (Avar effective_axis:tag:Avar ".":preds)) context position last "" env fncs db stmts
      Ast "step" (Avar "descendant_any":Ast "tags" tags:e:preds)
          -> do vs <- evalM e context position last effective_axis env fncs db stmts
                let ts = map (\(Avar tag) -> tag) tags
                if vs==[XNull]
                      then return vs
                      else foldr (\x r -> (liftM2 (++)) (applyPredicatesM preds (descendant_any_with_tagged_children ts x)
                                                                          True env fncs db stmts) r)
                                 (return []) vs
      Ast "step" (Avar step:Astring tag:e:preds)
          -> let step_fnc = findV step pathFunctions
             in do vs <- evalM e context position last effective_axis env fncs db stmts
                   if vs==[XNull]
                      then return vs
                      else foldr (\x r -> (liftM2 (++)) (applyPredicatesM preds (step_fnc tag x)
                                                                          True env fncs db stmts) r)
                                 (return []) vs
      Ast "filter" (e:preds)
          -> do vs <- evalM e context position last effective_axis env fncs db stmts
                applyPredicatesM preds vs True env fncs db stmts
      Ast "predicate" [condition,body]
          -> do eb <- evalM condition undefv1 undefv2 undefv3 "" env fncs db stmts
                if conditionTest eb
                   then evalM body context position last effective_axis env fncs db stmts
                   else return []
      Ast "executeSQL" [Avar var,args]
          -> do as <- evalM args context position last effective_axis env fncs db stmts
                executeSQL (findV var stmts) as
      Ast "append" args
          -> (liftM appendText) (mapM (\x -> evalM x context position last effective_axis env fncs db stmts) args)
      Ast "if" [c,t,e]     -- this is the only lazy function
          -> do ce <- evalM c context position last effective_axis env fncs db stmts
                evalM (if conditionTest ce then t else e) context position last effective_axis env fncs db stmts
      Ast "insert" [e1,e2]
          -> do v1 <- evalM e1 context position last effective_axis env fncs db stmts
                v2 <- evalM e2 context position last effective_axis env fncs db stmts
                insertDB db v1 v2
      Ast "delete" [e]
          -> do v <- evalM e context position last effective_axis env fncs db stmts
                deleteDB db v
      Ast "replace" [e1,e2]
          -> do v1 <- evalM e1 context position last effective_axis env fncs db stmts
                v2 <- evalM e2 context position last effective_axis env fncs db stmts
                replaceDB db v1 v2
      Ast "call" (v@(Avar fname):args)        -- Note: strict function application
          -> case filter (\(n,_,_,_) -> n == fname || ("fn:"++n) == fname) systemFunctions of
               [] -> do vs <- mapM (\a -> evalM a context position last effective_axis env fncs db stmts) args
                        if isBuildInType fname  && length vs == 1
                           then return $! castAs (head vs) v
                           else case filter (\(n,_,_) -> n == fname) fncs of
                                  (_,params,body):_
                                      -> if (length params) == (length args)
                                         then evalM body context undefv2 undefv3 ""
                                                    ((zip params vs)++env) fncs db stmts
                                         else error ("Wrong number of arguments in function call: "++fname)
                                  _ -> error ("Undefined function: "++fname)
               fs -> case filter (\(_,len,_,_) -> len < 0 || length args == len) fs of
                       [] -> error ("wrong number of arguments in function call: " ++ fname)
                       (_,_,f,_):_ -> do vs <- mapM (\x -> evalM x context position last effective_axis env fncs db stmts) args
                                         return $ f vs
      Ast "construction" [tag,id,parent,Ast "attributes" al,body]
             -> do ct <- evalM tag context position last effective_axis env fncs db stmts
                   bc <- evalM body context position last effective_axis env fncs db stmts
                   let (as,bs) = span (\x -> case x of XAttr _ _ -> True; _ -> False) bc
                   alc <- foldM (\r (Ast "pair" [a,v])
                                     -> do ac <- evalM a context position last effective_axis env fncs db stmts
                                           vc <- evalM v context position last effective_axis env fncs db stmts
                                           if vc==[XNull] then return r else return $! (qName ac,showXS vc):r) [] al
                   [XText vid] <- evalM id context position last effective_axis env fncs db stmts
                   vparent <- evalM parent context position last effective_axis env fncs db stmts
                   return $! [ XElem (qName ct) (alc ++ [ (n,v) | XAttr n v <- as ])
                                         (read vid) (if null vparent then parent_error else head vparent) bs ]
      Ast "attribute_construction" [name,value]
          -> do n <- evalM name context position last effective_axis env fncs db stmts
                v <- evalM value context position last effective_axis env fncs db stmts
                return $! [ XAttr (qName n) (showXS v) ]
      Ast "let" [Avar var,source,body]
          -> do s <- evalM source context position last effective_axis env fncs db stmts
                evalM body context position last effective_axis ((var,s):env) fncs db stmts
      Ast "for" [Avar var,Avar "$",source,body]      -- a for-loop without an index
          -> do vs <- evalM source context position last effective_axis env fncs db stmts
                foldr (\a r -> (liftM2 (++)) (evalM body a undefv2 undefv3 "" ((var,[a]):env) fncs db stmts) r)
                      (return []) vs
      Ast "for" [Avar var,Avar ivar,source,body]     -- a for-loop with an index
          -> do let p = maxPosition (Avar ivar) body
                    ns = if p > 0              -- there is a top-k like restriction
                            then Ast "step" [source,Ast "call" [Avar "<=",pathPosition,Aint p]]
                            else source 
                vs <- evalM ns context position last effective_axis env fncs db stmts
                foldir (\a i r -> (liftM2 (++)) (evalM body a i undefv3 "" ((var,[a]):(ivar,[XInt i]):env) fncs db stmts) r)
                       (return []) vs 1
      Ast "sortTuple" (exp:orderBys)             -- prepare each FLWOR tuple for sorting
          -> do vs <- evalM exp context position last effective_axis env fncs db stmts
                os <- mapM (\a -> evalM a context position last effective_axis env fncs db stmts) orderBys
                return $! [ XElem "" [] 0 parent_error (foldl (\r a -> r++[XElem "" [] 0 parent_error (text a)])
                                                              [XElem "" [] 0 parent_error vs] os) ]
      Ast "sort" (exp:ordList)                   -- blocking
          -> do vs <- evalM exp context position last effective_axis env fncs db stmts
                let ce = map (\(XElem _ _ _ _ xs) -> map (\(XElem _ _ _ _ ys) -> ys) xs) vs
                    ordering = foldr (\(Avar ord) r (x:xs) (y:ys)
                                       -> case compareXSeqs (ord == "ascending") x y of
                                            EQ -> r xs ys
                                            o -> o)
                                  (\xs ys -> EQ) ordList
                return $! (concatMap head (sortBy (\(_:xs) (_:ys) -> ordering xs ys) ce))
      Ast "type" [tp]
          -> return [ XType tp ]
      _ -> error ("Illegal XQuery: "++(show e))


-- evaluate from input continuously
evalInput :: (String -> Environment -> Functions -> IO(Environment,Functions)) -> Environment -> Functions -> String -> IO ()
evalInput eval vs fs prompt
    = do let bracs s = (length $ filter (== '{') s) - (length $ filter (== '}') s)
             oneline prompt = do line <- readline prompt
                                 case line of
                                   Nothing -> return ("quit",0)
                                   Just t -> if t == ""
                                             then oneline prompt
                                             else return $! (t,bracs t)
             readlines x c = do (line,bs) <- oneline ": "
                                if last line == '}' && bs+c == 0
                                   then return $! (x++" "++(init line),0)
                                   else if line == "quit"
                                        then return $! (line,0)
                                        else readlines (x++" "++line) (bs+c)
         (line,c) <- oneline prompt
         (stmt,_) <- if head line == '{'
                     then if last line == '}' && c==0
                          then return $! (init (tail line),0)
                          else readlines (tail line) c
                     else return $! (line,0)
         if stmt == "quit"
            then if prompt == "> " then putStrLn "Bye!" else putStrLn ""
            else do addHistory stmt
                    (nvs,nfs) <- eval stmt vs fs
                    evalInput eval nvs nfs prompt


debugSession :: XSeq -> Environment -> Functions -> Connection -> IO XSeq
debugSession e env fncs db
    = do let se = show e
         putStrLn $ "*** debug " ++ if null(index se 20) then se else (take 20 se) ++ " ..."
         putStr $ "Local variables:"
         mapM putStr (distinct $ map (\(v,_) -> " $"++v) env)
         putStrLn "\nYou may evaluate any XQuery. Type quit or ctr-D to continue execution."
         evalInput (\s vs fs -> do (result,nvs,nfs) <- xqueryE s vs fs db False
                                   putXSeq result
                                   return (nvs,nfs)) env fncs "debug> "
         return e


evalQueryM :: [Ast] -> Environment -> Functions -> Connection -> Bool -> IO (XSeq,Environment,Functions)
evalQueryM [] variables functions db verbose
    = return $! ([],variables,functions)
evalQueryM (query:xs) variables functions db verbose
    = case query of
        Ast "function" ((Avar f):body:args)
            -> do let opt = optimize body
                  if verbose
                     then do putStrLn "Abstract Syntax Tree (AST):"
                             putStrLn (ppAst body)
                             putStrLn "Optimized AST:"
                             putStrLn (ppAst opt)
                     else return ()
                  evalQueryM xs variables ((f,map (\(Avar v) -> v) args,opt):functions) db verbose
        Ast "variable" [Avar v,u]
            -> do uv <- evalM (optimize u) undefv1 undefv2 undefv3 "" variables functions db []
                  evalQueryM xs ((v,uv):variables) functions db verbose
        _ -> do let opt = optimize query
                    (ast,ns) = liftIOSources opt
                if verbose
                   then do putStrLn "Abstract Syntax Tree (AST):"
                           putStrLn (ppAst query)
                           putStrLn "Optimized AST:"
                           putStrLn (ppAst opt)
                           putStrLn "Result:"
                   else return ()
                env <- foldr (\(n,b,s) r -> case s of
                                              Avar m
                                                  -> do env <- r
                                                        return $! ((n,findV m env):env)
                                              Astring file
                                                  -> do doc <- uploadFile file
                                                        env <- r
                                                        return $! ((n,[materialize b (parseDocument doc)]):env)
                                              _ -> r)
                             (return []) ns
                stmts <- foldr (\(n,_,s) r -> case s of
                                                Ast "prepareSQL" [Astring sql]
                                                    -> do stmts <- r
                                                          t <- prepareSQL db sql
                                                          return $! ((n,t):stmts)
                                                _ -> r)
                               (return []) ns
                result <- evalM ast undefv1 undefv2 undefv3 "" (env++variables) functions db stmts
                (rest,renv,rfuns) <- evalQueryM xs variables functions db verbose
                return $! (result++rest,renv,rfuns)


xqueryE :: String -> Environment -> Functions -> Connection -> Bool -> IO (XSeq,Environment,Functions)
xqueryE query variables functions db verbose
    = evalQueryM (parse (scan query)) variables functions db verbose


-- | Evaluate the XQuery using the interpreter.
xquery :: String -> IO XSeq
xquery query = do (u,_,_) <- xqueryE query [] [] (error "No database connectivity") False
                  return $! u


-- | Evaluate the XQuery with database connectivity using the interpreter.
xqueryDB :: String -> Connection -> IO XSeq
xqueryDB query db = do (u,_,_) <- xqueryE query [] [] db False
                       return $! u


-- | Read an XQuery with database connectivity from a file and run it using the interpreter.
xfileDB :: String -> Connection -> IO XSeq
xfileDB file db = do query <- readFile file
                     xqueryDB query db
