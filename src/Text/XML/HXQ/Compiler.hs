{-------------------------------------------------------------------------------------
-
- A Compiler from XQuery to Haskell
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 02/15/08, last update: 01/04/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}

{-# OPTIONS -cpp #-}
{-# LANGUAGE TemplateHaskell #-}


module Text.XML.HXQ.Compiler
    ( xe, xq, xqdb, maxPosition, containsLast, qName, qx,
      parent_error, pathPosition, liftIOSources, uploadFile ) where

import Text.XML.HXQ.Parser
import Text.XML.HXQ.XTree
import Text.XML.HXQ.OptionalDB
import Control.Monad
import Char(toLower)
import List(sortBy)
import XMLParse(parseDocument)
import Text.XML.HXQ.Optimizer
import Text.XML.HXQ.Functions
import Text.XML.HXQ.Types
import Language.Haskell.TH
#if __GLASGOW_HASKELL__ >= 609
import Language.Haskell.TH.Quote
#endif

import Data.Char (intToDigit)
import Network.HTTP
import Network.URI
import System.Environment (getArgs)
--import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

err :: String -> IO a
err msg = do 
	  hPutStrLn stderr msg

get :: URI -> IO String
get uri =
    do
    eresp <- simpleHTTP (request uri)
    resp <- handleE (err . show) eresp
    case rspCode resp of
                      (2,0,0) -> return (rspBody resp)
                      _ -> err (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

request :: URI -> Request [Char]
request uri = Request{ rqURI = uri,
                       rqMethod = GET,
                       rqHeaders = [],
                       rqBody = "" }

handleE h (Left e) = h e
handleE _ (Right v) = return v

uploadFile :: String -> IO String
uploadFile uri = maybe (readFile uri) get (parseURI uri)


undef1 = [| error "Undefined XQuery context (.)" |]
undef2 = [| error "Undefined position()" |]
undef3 = [| error "Undefined last()" |]


-- does the expression contain a last()?
containsLast :: Ast -> Bool
containsLast (Ast "call" [Avar "last"]) = True
containsLast (Ast f _) | elem f ["let","for","predicate"] = False
containsLast (Ast "step" _) = False
containsLast (Ast _ args) = or (map containsLast args)
containsLast _ = False


liftAst :: Ast -> Q Exp
liftAst (Ast s xs)
    = let cs = listE $ map liftAst xs
      in [| Ast s $cs |]
liftAst (Avar v) = [| Avar v |]
liftAst (Aint n) = [| Aint n |]
liftAst (Astring s) = [| Astring s |]


-- calculate the maximum position value used in a predicate, if there is one
maxPosition :: Ast -> Ast -> Int
maxPosition position e
    = case e of
        Ast "call" [Avar f,p,n]
            | f `elem` ["=","<","<=","eq","lt","le"] && p == position && maxV n /= Nothing
            -> let Just k = maxV n in k
        Ast "call" [Avar f,n,p]
            | f `elem` ["=",">",">=","eq","gt","ge"] && p == position && maxV n /= Nothing
            -> let Just k = maxV n in k
        Ast "let" [Avar x,source,body]
            -> if position == Avar x
               then 0 else minp (maxPosition position source) (maxPosition position body)
        Ast "for" [Avar x,Avar i,source,body]
            -> if position == Avar x || position == Avar i
               then 0 else minp (maxPosition position source) (maxPosition position body)
        Ast "predicate" [pred,body]
            -> minp (maxPosition position pred) (maxPosition position body)
        Ast "call" [Avar "and",x,y]
            -> minp (maxPosition position x) (maxPosition position y)
        Ast "call" [Avar "or",x,y]
            -> max (maxPosition position x) (maxPosition position y)
        _ -> 0
    where minp x y = if x == 0 then y else if y == 0 then x else min x y
          maxV (Aint n) = Just n
          maxV (Ast "call" [Avar "to",_,n]) = maxV n
          maxV (Ast "call" [Avar "+",x,y]) = do n <- maxV x; m <- maxV y; return $ n+m
          maxV (Ast "call" [Avar "-",x,y]) = do n <- maxV x; m <- maxV y; return $ n-m
          maxV _ = Nothing


pathPosition = Ast "call" [Avar "position"]


parent_error = XError "constructed elements have no parent"


-- extract the QName
qName :: XSeq -> String
qName [XText s] = s
qName e = error ("Invalid QName: "++(show e))


makeAttribute :: XSeq -> XSeq -> [(String,String)]
makeAttribute ac vc
    = if vc==[XNull] then [] else [(qName ac,showXS vc)]


-- true, if it is a function in the IO monad
ioFunction :: String -> Q Bool
ioFunction name
    = do info <- reify (mkName name)
         iotp <- [t| IO XSeq |]
         case info of
           VarI _ (AppT _ tp) _ _
               | tp == iotp
               -> return True
           _ -> return False


-- make a function call
callF :: String -> [Q Exp] -> Q Exp
callF fname args
    = case filter (\(n,_,_,_) -> n == fname || ("fn:"++n)==fname) systemFunctions of
        [] ->     -- must be a Haskell function of type (XSeq,...,XSeq) -> IO XSeq
             let itp = case args of
                         [] -> [t| () |]
                         [_] -> [t| XSeq |]
                         _ -> foldr (\_ r -> appT r [t| XSeq |]) (appT (tupleT (length args)) [t| XSeq |])
                                    (tail args)
                 fn = sigE (varE (mkName fname))
                           (appT (appT arrowT itp) [t| IO XSeq |])
             in appE fn (tupE args)
        fs -> case filter (\(_,len,_,_) -> len < 0 || length args == len) fs of
                [] -> error ("wrong number of arguments in function call: " ++ fname)
                (_,_,_,f):_ -> f args


-- Each XPath predicate must calculate position() and last() from its input XSeq
-- if last() is used, then the evaluation is blocking (need to store the whole input XSeq)
compilePredicates :: [Ast] -> Q Exp -> Bool -> Q Exp
compilePredicates [] xs _ = xs
compilePredicates ((Aint n):preds) xs _   -- shortcut that improves laziness
    = compilePredicates preds
            [| index $xs $(litE (IntegerL (toInteger (n-1)))) |] True
compilePredicates ((Ast "call" [Avar "last"]):preds) xs _
    = compilePredicates preds [| [ last $xs ] |] True
compilePredicates (pred:preds) xs True    -- top-k like
    | maxPosition pathPosition pred > 0
    = compilePredicates (pred:preds)
           [| take $(litE (IntegerL (toInteger (maxPosition pathPosition pred)))) $xs |] False
compilePredicates (pred:preds) xs _
    | containsLast pred         -- blocking: use only when last() is used in the predicate
    = compilePredicates preds
            [| let bl = $xs
                   len = length bl
               in foldir (\x i r -> if case $(compile pred [| x |] [| [XInt i] |] [| [XInt len] |] "") of
                                         [XInt k] -> k == i               -- indexing
                                         b -> conditionTest b
                                    then x:r else r) [] bl 1 |] True
compilePredicates (pred:preds) xs _
    = compilePredicates preds
            [| foldir (\x i r -> if case $(compile pred [| x |] [| [XInt i] |] undef3 "") of
                                      [XInt k] -> k == i               -- indexing
                                      b -> conditionTest b
                                 then x:r else r) [] $xs 1 |] True


-- Compile the AST e into Haskell code
-- context: context node (XPath .)
-- position: the element position in the parent sequence (XPath position())
-- last: the length of the parent sequence (XPath last())
-- effective_axis: the XPath axis in /axis::tag(exp)
--        eg, the effective axis of //(A | B) is descendant
compile :: Ast -> Q Exp -> Q Exp -> Q Exp -> String -> Q Exp
compile e context position last effective_axis
  = case e of
      Avar "." -> [| [ $context :: XTree ] |]
      Avar v -> let x = varE (mkName v)
                in [| $x :: XSeq |]
      Aint n -> let x = litE (IntegerL (toInteger n))
                in [| [ XInt $x ] |]
      Afloat n -> let x = litE (RationalL (toRational n))
                  in [| [ XFloat $x ] |]
      Astring s -> let x = litE (StringL s)
                   in [| [ XText $x ] |]
      Ast "nonIO" [u] -> compile u context position last effective_axis
      Ast "context" [v,Astring dp,body]
          -> [| foldr (\x r -> $(compile body [| x |] position last dp)++r)
                      [] $(compile v context position last effective_axis) |]
      Ast "call" [Avar "position"]
          -> position
      Ast "call" [Avar "last"]
          -> last
      Ast "step" (Avar "child":tag:Avar ".":preds)
          | effective_axis /= ""
          -> compile (Ast "step" (Avar effective_axis:tag:Avar ".":preds)) context position last ""
      Ast "step" (Avar "descendant_any":Ast "tags" tags:e:preds)
          -> let bc = compile e context position last effective_axis
                 ts = listE (map (\(Avar tag) -> litE (stringL tag)) tags)
             in [| let v = $bc
                   in if v==[XNull]
                      then v
                      else foldr (\x r -> $(compilePredicates preds [| descendant_any_with_tagged_children $ts x |] True)++r)
                                 [] v |]
      Ast "step" (Avar step:Astring tag:e:preds)
          -> let bc = compile e context position last effective_axis
                 tc = litE (stringL tag)
             in [| let v = $bc
                   in if v==[XNull]
                      then v
                      else foldr (\x r -> $(compilePredicates preds [| $(findV step paths) $tc x |] True)++r)
                           [] v |]
      Ast "filter" (e:preds)
          -> compilePredicates preds (compile e context position last effective_axis) True
      Ast "predicate" [condition,body]
          -> [| if conditionTest $(compile condition undef1 undef2 undef3 "")
                then $(compile body context position last effective_axis)
                else [] |]
      Ast "append" args
          -> [| appendText $(listE (map (\x -> compile x context position last effective_axis) args)) |]
      Ast "if" [c,t,e]
          -> let ce = compile c context position last effective_axis
                 te = compile t context position last effective_axis
                 ee = compile e context position last effective_axis
             in [| if conditionTest $ce then $te else $ee |]
      Ast f _
          | elem f ["insert","delete","replace"]
          -> error "Updates must be over XML data stored in databases"
      Ast "call" [Avar fname,a]
          | isBuildInType fname
          -> let ac = compile a context position last effective_axis
             in [| castAs $ac (Avar fname) |]
      Ast "call" ((Avar f):_)
          | not (elem f system_functions)
          -> error "External function calls must be within the IO monad"
      Ast "call" ((Avar f):args)
          -> callF f (map (\x -> compile x context position last effective_axis) args)
      Ast "construction" [tag,id,parent,Ast "attributes" al,body]
          -> let alc = foldr (\(Ast "pair" [a,v]) r
                                  -> let ac = compile a context position last effective_axis
                                         vc = compile v context position last effective_axis
                                     in [| (makeAttribute $ac $vc) ++ $r |]) [| [] |] al
                 ct = compile tag context position last effective_axis
                 bc = compile body context position last effective_axis
                 cid = compile id context position last effective_axis
                 cparent = compile parent context position last effective_axis
             in [| let [XText vid] = $cid
                       vparent = $cparent
                       (as,bs) = span (\x -> case x of XAttr _ _ -> True; _ -> False) $bc
                       atts = $alc ++ [ (n,v) | XAttr n v <- as ]
                   in [ XElem (qName $ct) atts (read vid) (if null vparent then parent_error else head vparent) bs ] |]
      Ast "attribute_construction" [name,value]
          -> let ns = compile name context position last effective_axis
                 vs = compile value context position last effective_axis
             in [| [ XAttr (qName $ns) (showXS $vs) ] |]
      Ast "let" [Avar var,source,body]
          -> do s <- compile source context position last effective_axis
                b <- compile body context position last effective_axis
                return $! (AppE (LamE [VarP (mkName var)] b) s)
      Ast "for" [Avar var,Avar "$",source,body]      -- a for-loop without an index
          -> let b = compile body [| head $(varE (mkName var)) |] undef2 undef3 ""
                 f = lamE [varP (mkName var)] [| \r -> $b ++ r |]
                 s = compile source context position last effective_axis
             in [| foldr (\x -> $f [x]) [] $s |]
      Ast "for" [Avar var,Avar ivar,source,body]     -- a for-loop with an index
          -> let b = compile body [| head $(varE (mkName var)) |]
                             [| $(varE (mkName ivar)) |] undef3 ""
                 f = lamE [varP (mkName var)] (lamE [varP (mkName ivar)] [| \r -> $b ++ r |])
                 p = maxPosition (Avar ivar) body
                 ns = if p > 0              -- there is a top-k like restriction
                      then Ast "step" [source,Ast "call" [Avar "<=",pathPosition,Aint p]]
                      else source
                 s = compile ns context position last effective_axis
             in [| foldir (\x i -> $f [x] [XInt i]) [] $s 1 |]
      Ast "sortTuple" (exp:orderBys)             -- prepare each FLWOR tuple for sorting
          -> let res = foldl (\r a -> let ac = compile a context position last effective_axis
                                      in [| $r++[text $ac] |] )
                             [| [ $(compile exp context position last effective_axis) ] |] orderBys
             in [| [ $res ] |]
      Ast "sort" (exp:ordList)                   -- blocking
          -> let ce = compile exp context position last effective_axis
                 ordering = foldr (\(Avar ord) r
                                       -> let asc = if ord == "ascending"
                                                    then [| True |]
                                                    else [| False |]
                                          in [| \(x:xs) (y:ys) -> case compareXSeqs $asc x y of
                                                                    EQ -> $r xs ys
                                                                    o -> o |])
                                  [| \xs ys -> EQ |] ordList
             in [| concatMap head (sortBy (\(_:xs) (_:ys) -> $ordering xs ys) ($ce::[[XSeq]])) |]
      Ast "type" [tp]
          -> [| [ XType $(liftAst tp) ] |]
      _ -> error ("Illegal XQuery: "++(show e))


-- The monadic compilePredicates that propagates IO state
compilePredicatesM :: [Ast] -> Q Exp -> Bool -> Q Exp
compilePredicatesM [] xs _
    = [| return $xs |]
compilePredicatesM ((Aint n):preds) xs _   -- shortcut that improves laziness
    = compilePredicatesM preds
            [| index $xs $(litE (IntegerL (toInteger (n-1)))) |] True
compilePredicatesM ((Ast "call" [Avar "last"]):preds) xs _
    = compilePredicatesM preds [| [ last $xs ] |] True
compilePredicatesM (pred:preds) xs True    -- top-k like
    | maxPosition pathPosition pred > 0
    = compilePredicatesM (pred:preds)
           [| take $(litE (IntegerL (toInteger (maxPosition pathPosition pred)))) $xs |] False
compilePredicatesM (pred:preds) xs _
    | containsLast pred         -- blocking: use only when last() is used in the predicate
    = [| do let bl = $xs
                last = length bl
            vs <- foldir (\x i r -> do vs <- $(compileM pred [| x |] [| [XInt i] |] [| [XInt last] |] "")
                                       s <- r
                                       return (if case vs of
                                                    [XInt k] -> k == i               -- indexing
                                                    b -> conditionTest b
                                               then x:s else s))
                         (return []) $xs 1
            $(compilePredicatesM preds [| vs |] True) |]
compilePredicatesM (pred:preds) xs _
    = [| do vs <- foldir (\x i r -> do vs <- $(compileM pred [| x |] [| [XInt i] |] undef3 "")
                                       s <- r
                                       return (if case vs of
                                                    [XInt k] -> k == i               -- indexing
                                                    b -> conditionTest b
                                               then x:s else s))
                         (return []) $xs 1
            $(compilePredicatesM preds [| vs |] True) |]


-- The monadic XQuery compiler; it is like compile but has plumbing to propagate IO state
compileM :: Ast -> Q Exp -> Q Exp -> Q Exp -> String -> Q Exp
compileM e context position last effective_axis
  = case e of
      Avar "." -> [| return [ $context :: XTree ] |]
      Avar v -> let x = varE (mkName v)
                in [| return ($x :: XSeq) |]
      Aint n -> let x = litE (IntegerL (toInteger n))
                in [| return [ XInt $x ] |]
      Afloat n -> let x = litE (RationalL (toRational n))
                  in [| return [ XFloat $x ] |]
      Astring s -> let x = litE (StringL s)
                   in [| return [ XText $x ] |]
      -- for non-IO XQuery, use the regular compile
      Ast "nonIO" [u] -> [| return $(compile u context position last effective_axis) |]
      Ast "context" [v,Astring dp,body]
          -> [| do vs <- $(compileM v context position last effective_axis)
                   foldr (\x r -> (liftM2 (++)) $(compileM body [| x |] position last dp) r)
                         (return []) vs |]
      Ast "call" [Avar "position"]
          -> [| return $position |]
      Ast "call" [Avar "last"]
          -> [| return $last |]
      Ast "call" [Avar f,Astring file]
          | elem f ["doc","fn:doc"]
          -> [| do doc <- uploadFile file
                   return [materialize False (parseDocument doc)] |]
      Ast "step" (Avar "child":tag:Avar ".":preds)
          | effective_axis /= ""
          -> compileM (Ast "step" (Avar effective_axis:tag:Avar ".":preds)) context position last ""
      Ast "step" (Avar "descendant_any":Ast "tags" tags:e:preds)
          -> let bc = compileM e context position last effective_axis
                 ts = listE (map (\(Avar tag) -> litE (stringL tag)) tags)
             in [| do vs <- $bc
                      if vs==[XNull]
                         then return vs
                         else foldr (\x r -> (liftM2 (++)) $(compilePredicatesM preds
                                                               [| descendant_any_with_tagged_children $ts x |] True) r)
                                    (return []) vs |]
      Ast "step" (Avar step:Astring tag:e:preds)
          -> let bc = compileM e context position last effective_axis
                 tc = litE (stringL tag)
             in [| do vs <- $bc
                      if vs==[XNull]
                         then return vs
                         else foldr (\x r -> (liftM2 (++)) $(compilePredicatesM preds
                                                               [| $(findV step paths) $tc x |] True) r)
                                    (return []) vs |]
      Ast "filter" (e:preds)
          ->[| do vs <- $(compileM e context position last effective_axis)
                  $(compilePredicatesM preds [| vs |] True) |]
      Ast "predicate" [condition,body]
          -> [| do eb <- $(compileM condition undef1 undef2 undef3 "")
                   if conditionTest eb
                      then $(compileM body context position last effective_axis)
                      else return [] |]
      Ast "executeSQL" [Avar stmt,args]
          -> [| do as <- $(compileM args context position last effective_axis)
                   $(varE (mkName "executeSQL")) $(varE (mkName stmt)) as |]
      Ast "append" args
          -> let binds = zipWith (\i x -> (mkName ("x"++(show i)),x)) [1..(length args)] args
             in foldr (\(n,x) r -> [| $(compileM x context position last effective_axis) >>= $(lamE [varP n] r) |])
                      [| return (appendText $(listE (map (\(n,_) -> varE n) binds))) |] binds
      Ast "if" [c,t,e]
          -> let ce = compileM c context position last effective_axis
                 te = compileM t context position last effective_axis
                 ee = compileM e context position last effective_axis
             in [| do cond <- $ce
                      if conditionTest cond then $te else $ee |]
      Ast "insert" [e1,e2]
          -> let vc1 = compileM e1 context position last effective_axis
                 vc2 = compileM e2 context position last effective_axis
                 db = varE (mkName "_db")
             in [| do v1 <- $vc1
                      v2 <- $vc2
                      insertDB $db v1 v2 |]
      Ast "delete" [e]
          -> let vc = compileM e context position last effective_axis
                 db = varE (mkName "_db")
             in [| do v <- $vc
                      deleteDB $db v |]
      Ast "replace" [e1,e2]
          -> let vc1 = compileM e1 context position last effective_axis
                 vc2 = compileM e2 context position last effective_axis
                 db = varE (mkName "_db")
             in [| do v1 <- $vc1
                      v2 <- $vc2
                      replaceDB $db v1 v2 |]
      Ast "call" [Avar fname,a]
          | isBuildInType fname
          -> let ac = compileM a context position last effective_axis
             in [| do a <- $ac
                      return $! castAs a (Avar fname) |]
      Ast "call" ((Avar f):args)           -- Note: strict function application
          -> let binds = zipWith (\i x -> (mkName ("x"++(show i)),x)) [1..(length args)] args
                 call = if elem f system_functions
                        then [| return $(callF f (map (\(n,_) -> varE n) binds)) |]
                        else callF f (map (\(n,_) -> varE n) binds)
             in foldr (\(n,x) r -> [| $(compileM x context position last effective_axis) >>= $(lamE [varP n] r) |])
                      call binds
      Ast "construction" [tag,id,parent,Ast "attributes" al,body]
          -> let alc = foldr (\(Ast "pair" [a,v]) r
                                  -> [| do ac <- $(compileM a context position last effective_axis)
                                           vc <- $(compileM v context position last effective_axis)
                                           s <- $r
                                           return ((makeAttribute ac vc)++s) |]) [| return [] |] al
                 ct = compileM tag context position last effective_axis
                 bc = compileM body context position last effective_axis
                 cid = compile id context position last effective_axis
                 cparent = compile parent context position last effective_axis
             in [| do a <- $alc
                      c <- $ct
                      b <- $bc
                      let (as,bs) = span (\x -> case x of XAttr _ _ -> True; _ -> False) b
                          atts = a ++ [ (n,v) | XAttr n v <- as ]
                          [XText vid] = $cid
                          vparent = $cparent
                      return [ XElem (qName c) atts (read vid) (if null vparent then parent_error else head vparent) bs ] |]
      Ast "attribute_construction" [name,value]
          -> let ns = compileM name context position last effective_axis
                 vs = compileM value context position last effective_axis
             in [| do n <- $ns
                      v <- $vs
                      return $! [ XAttr (qName n) (showXS v) ] |]
      Ast "let" [Avar var,source,body]
          -> [|  $(compileM source context position last effective_axis)
                 >>= $(lamE [varP (mkName var)] (compileM body context position last effective_axis)) |]
      Ast "for" [Avar var,Avar "$",source,body]      -- a for-loop without an index
          -> let b = compileM body [| head $(varE (mkName var)) |] undef2 undef3 ""
                 f = lamE [varP (mkName var)] [| (liftM2 (++)) $b |]
                 s = compileM source context position last effective_axis
             in [| do vs <- $s
                      foldr (\x -> $f [x]) (return []) vs |]
      Ast "for" [Avar var,Avar ivar,source,body]     -- a for-loop with an index
          -> let b = compileM body [| head $(varE (mkName var)) |]
                             [| $(varE (mkName ivar)) |] undef3 ""
                 f = lamE [varP (mkName var)] (lamE [varP (mkName ivar)] [| (liftM2 (++)) $b |])
                 p = maxPosition (Avar ivar) body
                 ns = if p > 0              -- there is a top-k like restriction
                      then Ast "step" [source,Ast "call" [Avar "<=",pathPosition,Aint p]]
                      else source
                 s = compileM ns context position last effective_axis
             in [| do vs <- $s
                      foldir (\x i -> $f [x] [XInt i]) (return []) vs 1 |]
      Ast "sortTuple" (exp:orderBys)             -- prepare each FLWOR tuple for sorting
          -> let vs = compileM exp context position last effective_axis
                 res = foldl (\r a -> [| do ac <- $(compileM a context position last effective_axis)
                                            s <- $r
                                            return (s++[text ac]) |] )
                             [| do v <- $vs; return [ v ] |] orderBys
             in [| do r <- $res; return [r] |]
      Ast "sort" (exp:ordList)                   -- blocking
          -> let ce = compileM exp context position last effective_axis
                 ordering = foldr (\(Avar ord) r
                                       -> let asc = if ord == "ascending"
                                                    then [| True |]
                                                    else [| False |]
                                          in [| \(x:xs) (y:ys) -> case compareXSeqs $asc x y of
                                                                    EQ -> $r xs ys
                                                                    o -> o |])
                                  [| \xs ys -> EQ |] ordList
             in [| do c <- $ce
                      return (concatMap head (sortBy (\(_:xs) (_:ys) -> $ordering xs ys) (c::[[XSeq]]))) |]
      Ast "type" [tp]
          -> [| return [ XType $(liftAst tp) ] |]
      _ -> error ("Illegal XQuery: "++(show e))


-- functions that need IO interaction (document reader, DB access, etc)
ioSources :: [ String ]
ioSources
    = is ++ map (\x -> "fn:"++x) is
      where is = ["debug","executeSQL","doc","sql","publish","insert","delete","replace"]


-- steps that need the parent XTree link in evaluation (with a potential space leak)
backward_steps :: [ String ]
backward_steps = ["following-sibling", "following","parent", "ancestor", "preceding-sibling", "preceding", "ancestor-or-self" ]


-- Collect all input documents and assign them a unique number.
-- The backward flag indicates whether there are backward steps
-- (so that they would require XTrees with parent links)
pullIOSources :: Ast -> Int -> Bool -> (Ast, Int, Bool, [(String, Bool, Ast)])
pullIOSources query count backward
    = case query of
             Ast "call" [Avar nm,file]
                 | elem nm ["doc","fn:doc"]
                 -> (Avar ("_doc"++(show count)), count+1, backward, [("_doc"++(show count),backward,file)])
             Ast "call" [Avar nm,sql]
                 | elem nm ["sql","fn:sql"]
                 -> (Ast "executeSQL" [Avar ("_sql"++(show count)),Ast "call" [Avar "empty"]], count+1,
                     backward, [("_sql"++(show count),backward,Ast "prepareSQL" [sql])])
             Ast "call" [Avar nm,sql,args]
                 | elem nm ["sql","fn:sql"]
                 -> (Ast "executeSQL" [Avar ("_sql"++(show count)),args], count+1, backward,
                     [("_sql"++(show count),backward,Ast "prepareSQL" [sql])])
             Ast "step" (args@(Avar step:_))        -- backward step
                 | elem step backward_steps
                 -> let (s,c,ns) = foldr (\a r c -> let (e,c1,_,n1) = pullIOSources a c True
                                                        (s,c2,n2) = r c1
                                                    in (e:s,c2,union n1 n2))
                                         (\c -> ([],c,[])) args count
                    in (Ast "step" s,c,True,ns)
             Ast n args
                 -> let (s,c,ns) = foldr (\a r c -> let (e,c1,_,n1) = pullIOSources a c backward
                                                        (s,c2,n2) = r c1
                                                    in (e:s,c2,union n1 n2))
                                         (\c -> ([],c,[])) args count
                    in (Ast n s,c,backward,ns)
             _ -> (query,count,backward,[])
    where union xs ((n,b,s):ys) = (n,b,foldr(\(m,_,d) r -> if s==d && take 4 m /= "_sql" then Avar m else r) s xs):(union xs ys)
          union xs [] = xs


-- true if there is no need to lift to the IO monad
noIO :: Ast -> Bool
noIO (Ast nm _) | elem nm ioSources = False
noIO (Ast "call" (Avar nm:_))
    | elem nm ioSources || not (elem nm system_functions || isBuildInType nm)
    = False
noIO (Ast n args) = all noIO args
noIO _ = True


liftIOSources :: Ast  -> (Ast, [(String, Bool, Ast)])
liftIOSources query
    = let (ast,_,_,ns) = pullIOSources query 0 False
          f x = case x of
                  Ast nm _ | elem nm ["attributes","tags"] -> x
                  Ast _ _ | noIO x -> Ast "nonIO" [x]
                  _ -> case x of
                         Ast "call" ((Avar nm):args)
                             -> Ast "call" ((Avar nm):(map f args))
                         Ast n args -> Ast n (map f args)
                         _ -> x
      in (f ast,ns)


-- optimize and compile an AST 
compileAst :: Ast -> Q Exp
compileAst ast = compile (optimize ast) undef1 undef2 undef3 ""

compileAstM :: Ast -> Q Exp
compileAstM ast = compileM (optimize ast) undef1 undef2 undef3 ""


-- Compile an XQuery AST that does not perform IO (unlifted).
-- When evaluated, it returns XSeq.
compileQuery :: [Ast] -> Q Exp
compileQuery ((Ast "function" ((Avar f):b:args)):xs)
    = error "external function declarations are not permitted here"
compileQuery ((Ast "variable" [Avar v,u]):xs)
    = letE [valD (varP (mkName v)) (normalB (compileAst u)) []]
           (compileQuery xs)
compileQuery (query:xs)
    = let code = compileAst query
          rest = compileQuery xs
      in [| $code ++ $rest |]
compileQuery [] = [| [] |]


-- Compile an XQuery AST that may read XML documents or use databases (IO lifted).
-- When evaluated, it returns IO XSeq.
compileQueryM :: [Ast] -> Q Exp
compileQueryM ((Ast "function" ((Avar f):b:args)):xs)
    = let lvars = case args of
                    [Astring a] -> [varP (mkName a)]
                    _ -> [tupP (map (\(Avar a) -> varP (mkName a)) args)]
      in letE [valD (varP (mkName f)) (normalB (lamE lvars (compileAstM b))) []]
              (compileQueryM xs)
compileQueryM ((Ast "variable" [Avar v,u]):xs)
    = [| $(compileAstM u) >>= $(lamE [varP (mkName v)] (compileQueryM xs)) |]
compileQueryM (query:xs)
    = let (ast,ns) = liftIOSources (optimize query)
          code = compileM ast undef1 undef2 undef3 ""
          rest = compileQueryM xs
      in foldl (\r (n,b,e) -> let d = lamE [varP (mkName n)] r
                                  bc = if b then [| True |] else [| False |]  -- needed due to a bug in template haskell
                              in case e of
                                   Avar m -> [| $d $(varE (mkName m)) |]
                                   Ast "prepareSQL" [Astring sql]
                                       -> [| ($(varE (mkName "prepareSQL"))
                                                     $(varE (mkName "_db"))
                                                     $(litE (StringL sql))) >>= $d |]
                                   _ -> [| do let [XText f] = $(compileAst e)
                                              doc <- uploadFile f
                                              $d [materialize $bc (parseDocument doc)] |])
               [| (liftM2 (++)) $code $rest |] ns
compileQueryM [] = [| return [] |]


-- | Compile an XQuery expression that does not perform IO.
-- When the compiled code is evaluated, it returns a value of type @XSeq@.
xe :: String -> Q Exp
xe query = compileQuery (parse (scan query))


-- | Compile an XQuery that may perform IO (such as reading an XML document or calling a user function).
-- When the compiled code is evaluated, it returns a value of type @IO XSeq@.
xq :: String -> Q Exp
xq query = compileQueryM (parse (scan query))


-- | Compile an XQuery that may perform IO and/or queries a database.
-- When the compiled code is evaluated, it returns @Connection -> IO XSeq@.
xqdb :: String -> Q Exp
xqdb query = lamE [varP (mkName "_db")] (compileQueryM (parse (scan query)))


#if __GLASGOW_HASKELL__ >= 609
-- | Quasi-quotation for HXQ. For example, @[qx| doc(\"data\/cs.xml\")\/\/gpa |]@ is equivalent to @xq \"doc(\\\"data\/cs.xml\\\")\/\/gpa\"@.
qx = QuasiQuoter xq (\_ -> error "XQuery patterns are not allowed")
#else
type QuasiQuoter = String

-- | Quasi-quotation for HXQ (for ghc 6.09 or later). For example, @[qx| doc(\"data\/cs.xml\")\/\/gpa |]@ is equivalent to @xq \"doc(\\\"data\/cs.xml\\\")\/\/gpa\"@.
qx :: QuasiQuoter
qx = error "Quasi-quotation not permitted in earlier ghc versions"
#endif
