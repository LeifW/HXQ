{-------------------------------------------------------------------------------------
-
- Preprocess abstract syntax trees, remove backward steps, and optimize
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 05/01/08, last update: 01/05/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}


module Text.XML.HXQ.Optimizer(optimize) where

import Control.Monad
import Char(toLower)
import List(union)
import HXML(AttList)
import Text.XML.HXQ.Parser
import Text.XML.HXQ.XTree
import Text.XML.HXQ.OptionalDB


empty = Ast "call" [Avar "empty"]
true = Ast "call" [Avar "true"]
false = Ast "call" [Avar "false"]


distinct :: Eq a => [a] -> [a]
distinct = foldl (\r a -> if elem a r then r else r++[a]) []


-- collect attribute constructions inside element constructions
collect_attributes :: Ast -> (Ast,[Ast])
collect_attributes (Ast "attribute_construction" [attr,value])
    = (Ast "call" [Avar "empty"],[Ast "pair" [attr,value]])
collect_attributes (Ast "call" [Avar "concatenate",x,y])
    = let (cx,ax) = collect_attributes x
          (cy,ay) = collect_attributes y
      in (Ast "call" [Avar "concatenate",cx,cy],ax++ay)
collect_attributes (Ast "append" es)
    = let (s,a) = foldr (\e (r,ar) -> let (cx,ax) = collect_attributes e in (cx:r,ax++ar)) ([],[]) es
      in (Ast "append" s,a)
collect_attributes (Ast "step" (step:tag:e:preds))
    = let (ce,ae) = collect_attributes e
      in (Ast "step" (step:tag:ce:preds),ae)
collect_attributes e = (e,[])


-- does the expression contain a $var/.. term?
parentOfVar :: Ast -> String -> Bool
parentOfVar (Ast "step" [Avar "parent",_,Avar x]) var = x == var
parentOfVar (Ast "let" [Avar v,s,_]) var | var == v = parentOfVar s var
parentOfVar (Ast "for" [Avar v,Avar i,s,_]) var | var == v || var == i = parentOfVar s var
parentOfVar (Ast _ args) var = or (map (\x -> parentOfVar x var) args)
parentOfVar _ _ = False


-- replace $var/.. with $nvar
replaceParentOfVar :: Ast -> String -> String -> Ast
replaceParentOfVar (Ast "step" [Avar "parent",Astring "*",Avar x]) var nvar
    | x == var
    = Avar nvar
replaceParentOfVar (Ast "step" [Avar "parent",Astring tag,Avar x]) var nvar
    | x == var
    = Ast "step" [Avar "self",Astring tag,Avar nvar]
replaceParentOfVar (Ast "let" [Avar v,s,b]) var nvar | var == v
    = Ast "let" [Avar v,replaceParentOfVar s var nvar,b]
replaceParentOfVar (Ast "for" [Avar v,Avar i,s,b]) var nvar | var == v || var == i
    = Ast "for" [Avar v,Avar i,replaceParentOfVar s var nvar,b]
replaceParentOfVar (Ast f args) var nvar
    = Ast f (map (\x -> replaceParentOfVar x var nvar) args)
replaceParentOfVar e _ _ = e


-- Rules to extract the parent of an XQuery expression
-- For every XQuery x and predicates p1 ... pn and for s in [tag,*,@attr]:
--    x/s[p1]...[pn]/..   ->  x[s[p1]...[pn]]
--    x//s[p1]...[pn]/..  ->  x//*[s[p1]...[pn]]
removeParent :: Ast -> Maybe (Ast,Ast,Bool,Ast)
removeParent (Ast "predicate" [c,x])
    = do (nx,cond,childp,tag) <- removeParent x
         return (Ast "predicate" [c,nx],cond,childp,tag)
removeParent (Ast "step" (Avar "self":tag:x:preds))
    = do (nx,cond,childp,t) <- removeParent x
         return (Ast "step" (Avar "self":tag:nx:preds),cond,childp,t)
removeParent (Ast "step" (Avar "child":tag:x:preds))
    = Just (Ast "step" (Avar "child":tag:Avar ".":preds),x,True,tag)

removeParent (Ast "step" (Avar "descendant-or-self":tag:x:preds))
    = Just (Ast "step" (Avar "child":tag:Avar ".":preds),
            Ast "step" [Avar "descendant-or-self",Astring "*",x],True,tag)
removeParent (Ast "step" (Avar "descendant":tag:x:preds))
    = Just (Ast "step" (Avar "child":tag:Avar ".":preds),
            Ast "step" [Avar "descendant-or-self",Astring "*",x],True,tag)
removeParent (Ast "step" (Avar "attribute":tag:x:preds))
    = Just (Ast "step" (Avar "attribute":tag:Avar ".":preds),x,False,tag)
removeParent (Ast "step" (Avar "attribute-descendant":tag:x:preds))
    = Just (Ast "step" (Avar "attribute":tag:Avar ".":preds),
            Ast "step" [Avar "descendant-or-self",Astring "*",x],False,tag)
removeParent (Ast "step" (Avar "ancestor-or-self":tag:x:preds))
    = Just (true,Ast "step" (Avar "ancestor":tag:x:preds),False,tag)
removeParent (Ast "step" (Avar "preceding-sibling":tag:x:preds))
    = do (nx,cond,childp,t) <- removeParent x
         return (Ast "step" (Avar "child":tag:Avar ".":preds),cond,childp,t)
removeParent (Ast "step" (Avar "following-sibling":tag:x:preds))
    = do (nx,cond,childp,t) <- removeParent x
         return (Ast "step" (Avar "child":tag:Avar ".":preds),cond,childp,t)
removeParent e = Nothing


-- to speed up //* step, find possible immediate tagged children, if any (eg, x in //*/x)
tagged_children :: String -> Ast -> [String]
tagged_children context (Ast "step" (Avar "child":Astring tag:Avar v:_))
    | v == context
    = [tag]
tagged_children _ (Ast "step" _) = []
tagged_children context (Ast "let" [Avar var,source,body])
    = if context == "." || context == var
      then tagged_children context source
      else (tagged_children context source)++(tagged_children context body)
tagged_children context (Ast "for" [Avar var,Avar ivar,source,body])
    = if context == "." || context == var || context == ivar
      then tagged_children context source
      else (tagged_children context source)++(tagged_children context body)
tagged_children context (Ast _ xs) = concatMap (tagged_children context) xs
tagged_children _ _ = []


-- Preprocessing and simplification of ASTs
simplify :: Ast -> Ast
-- must be done bottom-up:    /../..
simplify (Ast "step" [Avar "parent",t,z@(Ast "step" [Avar "parent",_,x])])
    = let nz = simplify z
      in simplify (Ast "step" [Avar "parent",t,nz])
-- get rid of a parent step
simplify (Ast "step" (Avar "parent":tag:x:preds))
    = case removeParent x of
        Just (cond,nx,_,_)
            -> Ast "step" (Avar "self":tag:simplify nx:simplify cond:preds)
        Nothing -> Ast "step" (Avar "parent":tag:simplify x:map simplify preds)
-- remove $var/.. in a let-FLWOR
simplify (Ast "let" [Avar var,source,body])
    | parentOfVar body var
    = case removeParent source of
        Just (cond,nx,childp,tag)
            -> simplify (Ast "let" [Avar (var++"_parent"),Ast "step" (Avar "self":Astring "*":nx:[cond]),
                                    Ast "let" [Avar var,
                                               Ast "step" [ Avar (if childp
                                                                  then "child"
                                                                  else "attribute"),
                                                            tag, Avar (var++"_parent") ],
                                               replaceParentOfVar body var (var++"_parent")]])
        Nothing -> Ast "let" [Avar var,simplify source,simplify body]
-- remove $var/.. from a for-FLWOR
simplify (Ast "for" [Avar var,Avar "$",source,body])
    | parentOfVar body var
    = case removeParent source of
        Just (cond,nx,childp,tag)
            -> simplify (Ast "for" [Avar (var++"_parent"),Avar "$",Ast "step" (Avar "self":Astring "*":nx:[cond]),
                                    Ast "for" [Avar var,Avar "$",
                                               Ast "step" [ Avar (if childp
                                                                  then "child"
                                                                  else "attribute"),
                                                            tag, Avar (var++"_parent") ],
                                               replaceParentOfVar body var (var++"_parent")]])
        Nothing -> Ast "for" [Avar var,Avar "$",simplify source,simplify body]
-- pull out attributes from a general element construction
simplify (Ast "element_construction" [tag,Ast "attributes" as,content])
    = case collect_attributes content of
        (nc,(Ast "pair" [Astring "_id",id]):(Ast "pair" [Astring "_parent",parent]):attrs)
            -> simplify (Ast "construction" [tag,id,parent,Ast "attributes" (as++attrs),nc])
	(nc,attrs) -> simplify (Ast "construction" [tag,Astring "0",Ast "call" [Avar "empty"],Ast "attributes" (as++attrs),nc])
-- if //* collect all children tagnames to use descendant_any
simplify (Ast "for" [Avar var,i,Ast "step" (Avar "descendant":Astring "*":path:preds),body])
    | not (null ((tagged_children var body))) || any (not . null . (tagged_children ".")) preds
    = let ctags = distinct ((tagged_children var body)++(concatMap (tagged_children ".") preds))
          tags = Ast "tags" (map Avar ctags)
      in simplify (Ast "for" [Avar var,i,Ast "step" (Avar "descendant_any":tags:path:preds),body])
simplify (Ast "step" (Avar "child":Astring tag:Ast "step" (Avar "descendant":Astring "*":path:preds):preds2))
    = let ctags = distinct(tag:(concatMap (tagged_children ".") preds))
          tags = Ast "tags" (map Avar ctags)
      in simplify (Ast "step" (Avar "child":Astring tag:Ast "step" (Avar "descendant_any":tags:path:preds):preds2))
simplify (Ast "step" (Avar "descendant":Astring "*":path:preds))
    | any (not . null . (tagged_children ".")) preds
    = let ctags = distinct (concatMap (tagged_children ".") preds)
          tags = Ast "tags" (map Avar ctags)
      in simplify (Ast "step" (Avar "descendant_any":tags:path:preds))
-- expand the wrapper of a stored document
simplify (Ast "call" [Avar "publish",Astring dbpath,Astring name])
    = simplify (publishXmlDoc dbpath name False)
-- default
simplify (Ast n args) = Ast n (map simplify args)
simplify e = e


-- simplify e/tag
taggedElement :: [Ast] -> String -> Maybe [Ast]
taggedElement (e@(Ast "construction" [Astring ctag,_,_,_,x]):xs) tag
    | ctag == tag || tag == "*"
    = do s <- taggedElement xs tag
         return (e:s)
taggedElement ((Ast "construction" [_,_,_,_,_]):xs) tag
    = taggedElement xs tag
taggedElement ((Ast "call" [Avar "concatenate",x,y]):xs) tag
    = do tx <- taggedElement (x:xs) tag
         ty <- taggedElement (y:xs) tag
         return (tx++ty)
taggedElement ((Astring _):xs) tag
    = taggedElement xs tag
taggedElement ((Aint _):xs) tag
    = taggedElement xs tag
taggedElement (e:xs) tag = Nothing
taggedElement [] _ = Just []


sqlComparisson = [("=","="),("eq","="),("<=","<="),(">=",">="),("!=","!="),(">",">"),
                  ("<","<"),("ne","!="),("gt",">"),("lt","<"),("ge",">="),("le","<=")]

sqlBoolean = [("and","and"),("or","or")]


-- Can this be transformed to an SQL predicate?
sqlPredicate :: [String] -> Ast -> Bool
sqlPredicate tables e
    = case e of
        Ast "step" (Avar "child":Astring tag:Avar v:preds)
            -> (elem v tables) && (all (sqlPredicate tables) preds)
        Ast "construction" [_,_,_,_,Ast "append" xs]
            -> all (sqlPredicate tables) xs
        Ast "call" [Avar "text",x]
            -> sqlPredicate tables x
        Ast "call" [Avar cmp,x,y]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> (sqlExpr tables x) && (sqlExpr tables y)
        Ast "call" [Avar cmp,x,y]
            | any (\(f,_) -> f==cmp) sqlBoolean
            -> (sqlPredicate tables x) && (sqlPredicate tables y)
        _ -> False
      where sqlExpr tables e
                = case e of
                    Astring s -> True
                    Aint n -> True
                    Ast "step" (Avar "child":Astring tag:Avar v:preds)
                        -> elem v tables
                    Ast "construction" [_,_,_,_,Ast "append" xs]
                        -> all (sqlExpr tables) xs
                    Ast "call" [Avar "text",x]
                        -> sqlExpr tables x
                    Ast "for" [Avar v,_,Ast "call" ((Avar "SQL"):_),x]
                        -> sqlExpr (v:tables) x
                    _ -> False


-- Convert a predicate AST to an SQL predicate that uses the tables
predToSQL :: [String] -> Ast -> (String,[Ast],[String])
predToSQL tables e
    = case e of
        Ast "step" [Avar "child",Astring tag,Avar v]
            -> if (elem v tables)
               then ("",[],[])
               else error ("Cannot convert to an SQL predicate: "++show e)
        Ast "step" (Avar "child":Astring tag:Avar v:pred:preds)
            -> if (elem v tables) && (all (sqlPredicate tables) preds)
               then foldl (\(p',ps',ts') (p,ps,ts) -> (p' ++ " and " ++ p,ps++ps,union ts' ts))
                          (predToSQL tables pred)
                          (map (predToSQL tables) preds)
               else error ("Cannot convert to an SQL predicate: "++show e)
        Ast "construction" [_,_,_,_,Ast "append" xs]
            -> orAll (map (predToSQL tables) xs)
        Ast "call" [Avar "text",x]
            -> predToSQL tables x
        Ast "call" [Avar cmp,Ast "for" [Avar v,i,Ast "call" [Avar "SQL",_,Ast "call" ((Avar "tables"):t),pred],x],y]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> let ts = [ x | Avar x <- t ]
                   (p,ps,ts') = predToSQL (tables++ts) (call "and" [Ast "call" [Avar cmp,x,y],pred])
               in (p,ps,union ts' ts)
        Ast "call" [Avar cmp,x,Ast "for" [Avar v,i,Ast "call" [Avar "SQL",_,Ast "call" ((Avar "tables"):t),pred]],y]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> let ts = [ x | Avar x <- t ]
                   (p,ps,ts') = predToSQL (tables++ts) (call "and" [Ast "call" [Avar cmp,x,y],pred])
               in (p,ps,union ts' ts)
        Ast "call" [Avar cmp,x,y]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> let (nx,vx,px,tx) = expToSQL tables x
                   (ny,vy,py,ty) = expToSQL tables y
                   p = if (null vx) && (null vy) then "" else foldl (\r p -> r++" and "++p) "" (px++py)
               in if nx == ""
                  then (ny,vx,union tx ty)
                  else if ny == ""
                       then (nx++p,vy,union tx ty)
                       else (nx ++ " " ++ snd (head (filter (\(f,_) -> f==cmp) sqlComparisson)) ++ " " ++ ny++p,vx++vy,union tx ty)
        Ast "call" [Avar cmp,x,y]
            | any (\(f,_) -> f==cmp) sqlBoolean
            -> let (nx,vx,tx) = predToSQL tables x
                   (ny,vy,ty) = predToSQL tables y
               in if nx == ""
                  then (ny,vy,union tx ty)
                  else if ny == ""
                       then (nx,vx,union tx ty)
                       else (nx ++ " " ++ snd (head (filter (\(f,_) -> f==cmp) sqlBoolean)) ++ " " ++ ny,vx++vy,union tx ty)
        _ -> error ("Cannot convert to an SQL predicate: "++show e)
      where expToSQL :: [String] -> Ast -> (String,[Ast],[String],[String])
            expToSQL tables e
                = case e of
                    Astring s -> ("\'"++s++"\'",[],[],[])
                    Aint n -> (show n,[],[],[])
                    Ast "step" [Avar "child",Astring tag,Avar v]
                        -> if elem v tables
                           then (v++"."++tag,[],[],[])
                           else ("?",[e],[],[])
                    Ast "step" (Avar "child":Astring tag:Avar v:pred:preds)
                        -> let (p,ps,ts) = foldl (\(p',ps',ts') (p,ps,ts) -> (p' ++ " and " ++ p,ps'++ps,union ts' ts))
                                                 (predToSQL tables pred)
                                                 (map (predToSQL tables) preds)
                           in if elem v tables
                              then (v++"."++tag,ps,[p],ts)
                              else ("?",e:ps,[p],ts)
                    Ast "construction" [_,_,_,_,Ast "append" [x]]
                        -> expToSQL tables x
                    Ast "call" [Avar "text",x]
                        -> expToSQL tables x
                    _ -> ("?",[e],[],[])
            orAll [x] = x
            orAll (x:xs) = foldl (\(a,as,at) (b,bs,bt) -> ("("++a++" or "++b++")",as++bs,union at bt)) x xs


-- Convert an AST to an SQL query
makeSQL :: [String] -> [Ast] -> Ast -> [Ast] -> (String,[Ast])
makeSQL tables fromTables pred cols
    = let tnames = [ x | Avar x <- fromTables ]
          ts = combine tnames
          cs = combine [ x | Avar x <- cols ]
          vars (Ast n args) = concatMap vars args
          vars (Avar v) | not (elem v tnames) = [v]
          vars _ = []
          combine [] = ""
          combine [x] = x
          combine (x:xs) = x++", "++combine xs
      in if pred == Ast "call" [Avar "true"]
         then (if null cs
               then "select * from "++ts
               else "select "++cs++" from "++ts,[])
         else let (p,args,nts) = predToSQL (tables++tnames) pred
                  pp = if p=="" then "" else " where "++p
              in (if null cs
                  then "select * from "++combine (union tnames nts)++pp
                  else "select "++cs++" from "++combine (union tnames nts)++pp,args)


findAttr :: String -> [Ast] -> Ast
findAttr tag ((Ast "pair" [n@(Astring a),v]):xs)
    | a==tag || tag=="*"
    = case findAttr tag xs of
        Ast "call" [Avar "empty"] -> Ast "attribute_construction" [n,v]
        z -> Ast "call" [Avar "concatenate",Ast "attribute_construction" [n,v],z]
findAttr tag (_:xs) = findAttr tag xs
findAttr _ [] = empty


andAll :: [Ast] -> Ast
andAll [] = true
andAll [x] = x
andAll (x:xs) = foldl (\a r -> call "and" [a,r]) x xs


orAll :: [Ast] -> Ast
orAll [] = true
orAll [x] = x
orAll (x:xs) = foldl (\a r -> call "or" [a,r]) x xs


occursContext :: Ast -> Int
occursContext e
    = case e of
        Avar "." -> 1
        Ast "let" _ -> 0
        Ast "for" _ -> 0
        Ast "call" [Avar "SQL",s,f,w]
            -> occursContext w
        Ast "step" (step:tag:x:preds)
            -> occursContext x
        Ast n xs -> sum (map occursContext xs)
        _ -> 0


substContext :: Ast -> Ast -> Ast
substContext e b
    = case b of
        Avar "." -> e
        Ast "let" _ -> b
        Ast "for" _ -> b
        Ast "call" [Avar "SQL",s,f,w]
            -> Ast "call" [Avar "SQL",s,f,substContext e w]
        Ast "step" (step:tag:x:preds)
            -> Ast "step" (step:tag:(substContext e x):preds)
        Ast n xs -> Ast n (map (substContext e) xs)
        _ -> b


occurs :: String -> Ast -> Int
occurs v e
    = case e of
        Avar w | v==w -> 1
        Ast "let" [Avar w,s,_] | v==w -> occurs v s
        Ast "for" [Avar w,Avar i,s,_] | v==w || v==i -> occurs v s
        Ast "call" [Avar "SQL",s,f,w]
            -> occurs v w
        Ast n xs -> sum (map (occurs v) xs)
        _ -> 0


subst :: String -> Ast -> Ast -> Ast
subst v e b
    = case b of
        Avar w | v==w -> e
        Ast "let" [Avar w,s,_] | v==w -> subst v e s
        Ast "for" [Avar w,Avar i,s,_] | v==w || v==i -> subst v e s
        Ast "call" [Avar "SQL",s,f,w]
            -> Ast "call" [Avar "SQL",s,f,subst v e w]
        Ast n xs -> Ast n (map (subst v e) xs)
        _ -> b


dependsOnPosition :: Bool -> Ast -> Bool
dependsOnPosition contextp e
    = case e of
        Avar "." -> contextp
        Ast "call" [Avar "position"] -> True
        Ast "call" [Avar "last"] -> True
        Ast "step" (step:tag:x:_)
            -> dependsOnPosition contextp x
        Ast _ xs -> any (dependsOnPosition contextp) xs
        _ -> False


wellFormedPredicate :: Bool -> Ast -> Bool
wellFormedPredicate contextp e
    = case e of
        Ast "step" (step:tag:x:preds)
            -> not (dependsOnPosition contextp x)
        Ast "construction" xs
            -> not (any (dependsOnPosition contextp) xs)
        Ast "call" [Avar "not",x]
            -> not (dependsOnPosition contextp x)
        Ast "call" [Avar cmp,x,y]
            | any (\(f,_) -> f==cmp) (sqlComparisson++sqlBoolean)
            -> not (dependsOnPosition contextp x)
               && not (dependsOnPosition contextp y)
        _ -> False


splitSqlPredicate :: [String] -> Ast -> Maybe (Ast,[Ast])
splitSqlPredicate tables (Ast "call" [Avar "and",p1,p2])
    = case (splitSqlPredicate tables p1,splitSqlPredicate tables p2) of
        (Nothing,Nothing) -> Nothing
        (Nothing,Just(pp1,pp2))
            -> Just(pp1,p1:pp2)
        (Just(pp1,pp2),Nothing)
            -> Just(pp1,p2:pp2)
        (Just(pp1,pp2),Just(pp3,pp4))
            -> Just(Ast "call" [Avar "and",pp1,pp3],pp2++pp4)
splitSqlPredicate tables pred
    | sqlPredicate tables pred
    = Just(pred,[])
splitSqlPredicate tables pred = Nothing


is_constant :: Ast -> Bool
is_constant (Astring _) = True
is_constant (Aint _) = True
is_constant (Afloat _) = True
is_constant _ = False


predicates :: Ast -> [Ast] -> Ast
predicates e [] = e
predicates e preds = Ast "step" (Avar "self":Astring "*":e:preds)


-- Normalization
normalize :: Ast -> Bool -> Int -> (Ast,Bool,Int)
normalize exp changed count
    = case exp of
        Ast "step" (step:tag:x:preds)
            | any (\p -> p==true) preds
            -> let preds' = filter (\p -> p /= true) preds
               in norm (Ast "step" (step:tag:x:preds'))
        Ast "step" (step:tag:x:preds)
            | any (\p -> p==false) preds
            -> (empty,True,count)
        Ast "step" [Avar "self",Astring "*",e]
            -> norm e
-- path steps over constants always give ()
        Ast "step" (step:tag:c:_)
            | is_constant c
            -> (empty,True,count)
        Ast "step" (step:tag:Ast "call" [Avar "text",_]:_)
            -> (empty,True,count)
        Ast "step" (step:tag:Ast "call" [Avar "empty"]:_)
            -> (empty,True,count)
-- boolean reductions
        Ast "call" [Avar "and",x,y]
            | x == false || y == false
            -> (false,True,count)
        Ast "call" [Avar "or",x,y]
            | x == true && y == true
            -> (true,True,count)
        Ast "call" [Avar "and",Ast "call" [Avar "true"],y]
            -> norm y
        Ast "call" [Avar "and",x,Ast "call" [Avar "true"]]
            -> norm x
        Ast "call" [Avar "or",Ast "call" [Avar "false"],y]
            -> norm y
        Ast "call" [Avar "or",x,Ast "call" [Avar "false"]]
            -> norm x
        Ast "call" [Avar "not",Ast "call" [Avar "true"]]
            -> (false,True,count)
        Ast "call" [Avar "not",Ast "call" [Avar "false"]]
            -> (true,True,count)
        -- (x,())  ->  x
        Ast "call" [Avar "concatenate",x,Ast "call" [Avar "empty"]]
            -> norm x
        -- ((),x)  ->  x
        Ast "call" [Avar "concatenate",Ast "call" [Avar "empty"],x]
            -> norm x
        Ast "append" ((Ast "call" [Avar "empty"]):xs)
            -> norm (Ast "append" xs)
        Ast "call" [Avar "=",x,y]
            | x == empty && y == empty
            -> (true,True,count)
        Ast "call" [Avar "=",x,y]
            | (x == empty && is_constant y) || (y == empty && is_constant x)
            -> (false,True,count)
        Ast "call" [Avar cmp,Ast "construction" [_,_,_,_,Ast "append" xs],y]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> norm (orAll (map (\x -> Ast "call" [Avar cmp,x,y]) xs))
        Ast "call" [Avar cmp,x,Ast "construction" [_,_,_,_,Ast "append" ys]]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> norm (orAll (map (\y -> Ast "call" [Avar cmp,x,y]) ys))
        Ast "call" [Avar cmp,Ast "for" [v,i,s,Ast "construction" [_,_,_,_,Ast "append" xs]],y]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> norm (orAll (map (\x -> Ast "call" [Avar cmp,Ast "for" [v,i,s,x],y]) xs))
        Ast "call" [Avar cmp,x,Ast "for" [v,i,s,Ast "construction" [_,_,_,_,Ast "append" ys]]]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> norm (orAll (map (\y -> Ast "call" [Avar cmp,x,Ast "for" [v,i,s,y]]) ys))
        Ast "call" [Avar cmp,Ast "call" [Avar "empty"],y]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> (false,True,count)
        Ast "call" [Avar cmp,x,Ast "call" [Avar "empty"]]
            | any (\(f,_) -> f==cmp) sqlComparisson
            -> (false,True,count)
-- normalize FLWORs
        Ast "for" [v,i,Ast "call" [Avar "empty"],b]
            -> (empty,True,count)
        Ast "for" [v,i,s,Ast "call" [Avar "empty"]]
            -> (empty,True,count)
        -- for $v1 in (for $v2 in s2 return b2) return b1  -->  for $v2 in s2, for $v1 in b2 return b1
        Ast "for" [v1,i1,Ast "for" [v2,i2,s2,b2],b1]
            -> norm (Ast "for" [v2,i2,s2,Ast "for" [v1,i1,b2,b1]])
        -- for $v in (x,y) return b  -->  (for $v in x return b,for $v in y return b)
        Ast "for" [v,i@(Avar "$"),Ast "call" [Avar "concatenate",x,y],b]
            -> norm (Ast "call" [Avar "concatenate",Ast "for" [v,i,x,b],Ast "for" [v,i,y,b]])
        -- for $v in <a>...</a> return b  -->  b[$v/(<a>...</a>)]
        Ast "for" [Avar v,Avar i,e@(Ast "construction" _),b]
            -> norm (if i == "$"
                     then subst v e b
                     else subst v e (subst i (Aint 1) b))
        Ast "for" [Avar v,Avar i,e,b]
            | is_constant e
            -> norm (if i == "$"
                     then subst v e b
                     else subst v e (subst i (Aint 1) b))
-- normalize XPath steps
        Ast "step" (Avar "self":Astring "*":Ast "step" (step:tag:x:preds2):preds1)
            -> norm (Ast "step" (step:tag:x:preds1++preds2))
        Ast "step" (step:tag:Ast "step" (Avar "self":Astring "*":x@(Ast "construction" _):preds1):preds2)
            -> let npreds1 = map (substContext x) preds1
               in norm (Ast "step" (step:tag:x:npreds1++preds2))
        Ast "step" (step:tag:x:(Ast "step" (Avar "self":Astring "*":y:preds2)):preds1)
            -> let npreds2 = map (substContext y) preds2
               in norm (Ast "step" (step:tag:x:preds1++npreds2))
        -- (for $v in s return b)/tag  -->  for $v in s return b/tag
        Ast "step" (step:tag:Ast "for" [v,i,s,b]:preds)
            | all (wellFormedPredicate False) preds
            -> norm (Ast "for" [v,i,s,Ast "step" (step:tag:b:preds)])
       -- promote well-formed predicates; but note:  (x,y)[1] <> (x[1],y[1])
        Ast "step" (step:tag:Ast "call" [Avar "concatenate",x,y]:preds)
            | all (wellFormedPredicate False) preds
            -> norm (Ast "call" [Avar "concatenate",
                                 Ast "step" (step:tag:x:preds),
                                 Ast "step" (step:tag:y:preds)])
        -- (<ctag>...<tag>...</tag>...</ctag>)/tag  -->  ...<tag>...</tag>...
        Ast "step" [Avar "child",Astring tag,Ast "construction" [_,_,_,_,Ast "append" x]]
            | taggedElement x tag /= Nothing
            -> case taggedElement x tag of
                 Just [] -> (empty,True,count)
                 Just s -> norm (concatenateAll s)
        Ast "step" (Avar "child":tag:Ast "construction" [ctag,_,_,al,Ast "append" x]:preds)
            -> norm (Ast "step" (Avar "self":tag:concatenateAll x:preds))
        Ast "step" (Avar "self":Astring tag:e@(Ast "construction" [Astring ctag,_,_,al,Ast "append" x]):preds)
            | tag /= "*"
            -> if tag == ctag
               then norm (Ast "step" (Avar "self":Astring "*":e:preds))
               else (empty,True,count)
        -- (<tag>x</tag>)//tag  --> (x,x//tag)
        Ast "step" (Avar "descendant_any":tags:z@(Ast "construction" [Astring ctag,_,_,al,Ast "append" x]):preds)
            -> norm (Ast "call" [Avar "concatenate",predicates z preds,
                                 Ast "step" (Avar "descendant_any":tags:concatenateAll x:preds)])
        Ast "step" (Avar "descendant":Astring tag:Ast "construction" [_,_,_,al,Ast "append" x]:preds)
            -> norm (Ast "step" (Avar "descendant-or-self":Astring tag:concatenateAll x:preds))
        Ast "step" (Avar "descendant-or-self":Astring tag:z@(Ast "construction" [Astring ctag,_,_,al,Ast "append" x]):preds)
            -> norm (if tag == ctag || tag == "*"
                     then Ast "call" [Avar "concatenate",predicates z preds,
                                      Ast "step" (Avar "descendant-or-self":Astring tag:concatenateAll x:preds)]
                     else Ast "step" (Avar "descendant-or-self":Astring tag:concatenateAll x:preds))
        -- (<tag A=s>x</tag>)/@A  --> s
        Ast "step" (Avar "attribute":Astring tag:Ast "construction" [ctag,_,_,Ast "attributes" as,x]:preds)
            -> norm (predicates (findAttr tag as) preds)
        -- (<tag A=s>x</tag>)//@A  --> (s,x//@A)
        Ast "step" (Avar "attribute-descendant":Astring tag:Ast "construction" [ctag,_,_,Ast "attributes" as,Ast "append" x]:preds)
            -> norm (Ast "call" [Avar "concatenate",predicates (findAttr tag as) preds,
                                 Ast "step" (Avar "attribute-descendant":Astring tag:concatenateAll x:preds)])
-- SQL folding
        Ast "for" [Avar v1,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s1),Ast "call" ((Avar "tables"):f1),pred1],
                   Ast "for" [Avar v2,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s2),Ast "call" ((Avar "tables"):f2),pred2],
                              b]]
            | occurs v1 b == 0
            -> norm (Ast "for" [Avar v2,Avar "$",
                                Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):(s1++s2)),
                                            Ast "call" ((Avar "tables"):(f1++f2)),Ast "call" [Avar "and",pred1,pred2]],
                                b])
        Ast "for" [Avar v,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s),Ast "call" ((Avar "tables"):tables),pred1],
                   Ast "step" (Avar "self":Astring "*":x:pred2)]
            | splitSqlPredicate [ v | Avar v <- tables ] (andAll pred2) /= Nothing
            -> let Just(pred3,pred4) = splitSqlPredicate [ v | Avar v <- tables ] (andAll pred2)
               in norm (Ast "for" [Avar v,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s),
                                                               Ast "call" ((Avar "tables"):tables),Ast "call" [Avar "and",pred1,pred3]],
                                   Ast "step" (Avar "self":Astring "*":x:pred4)])
        Ast "for" [Avar v,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s),Ast "call" ((Avar "tables"):tables),pred1],
                   Ast "step" (Avar "self":Astring "*":x:pred2)]
            | occursContext x == 0 && sum (map occursContext pred2) > 0
            -> norm (Ast "for" [Avar v,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s),Ast "call" ((Avar "tables"):tables),pred1],
                                Ast "step" (Avar "self":Astring "*":x:map (substContext x) pred2)])
        Ast "for" [Avar v1,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s1),Ast "call" ((Avar "tables"):f1),pred1],
                   Ast "for" [Avar v2,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s2),Ast "call" ((Avar "tables"):f2),pred2],
                              Ast "step" (Avar "self":Astring "*":b:predd)]]
            | occurs v1 b == 0 && splitSqlPredicate [ v | Avar v <- f1 ] (andAll predd) /= Nothing
            -> let Just(pred3,pred4) = splitSqlPredicate [ v | Avar v <- f1 ] (andAll predd)
               in norm (Ast "for" [Avar v1,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s1),
                                                                Ast "call" ((Avar "tables"):f1),Ast "call" [Avar "and",pred1,pred3]],
                                   Ast "for" [Avar v2,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s2),
                                                                           Ast "call" ((Avar "tables"):f2),pred2],
                                              Ast "step" (Avar "self":Astring"*":b:pred4)]])
        Ast "for" [Avar v,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s),Ast "call" ((Avar "tables"):tables),pred1],
                   Ast "predicate" [pred2,x]]
            | splitSqlPredicate [ v | Avar v <- tables ] pred2 /= Nothing
            -> let Just(pred3,pred4) = splitSqlPredicate [ v | Avar v <- tables ] pred2
               in norm (Ast "for" [Avar v,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s),
                                                               Ast "call" ((Avar "tables"):tables),Ast "call" [Avar "and",pred1,pred3]],
                                   Ast "step" (Avar "self":Astring"*":x:pred4)])
        Ast "for" [Avar v1,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s1),Ast "call" ((Avar "tables"):f1),pred1],
                   Ast "for" [Avar v2,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s2),Ast "call" ((Avar "tables"):f2),pred2],
                              Ast "predicate" [predd,b]]]
            | occurs v1 b == 0 && splitSqlPredicate [ v | Avar v <- f1 ] predd /= Nothing
            -> let Just(pred3,pred4) = splitSqlPredicate [ v | Avar v <- f1 ] predd
               in norm (Ast "for" [Avar v1,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s1),
                                                                Ast "call" ((Avar "tables"):f1),Ast "call" [Avar "and",pred1,pred3]],
                                   Ast "for" [Avar v2,Avar "$",Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):s2),
                                                                           Ast "call" ((Avar "tables"):f2),pred2],
                                              Ast "step" (Avar "self":Astring"*":b:pred4)]])
-- ignore children from insertion destination
        Ast "destination" [Ast "for" [v,i,Ast "call" (Avar "SQL":sql),body]]
            -> norm (Ast "for" [v,i,Ast "call" (Avar "SQL":sql),Ast "destination" [body]])
        Ast "destination" [Ast "construction" [t,id,p,al,_]]
            -> norm (Ast "construction" [t,id,p,al,Ast "append" []])
        Ast "destination" [Ast f args]
            -> norm (Ast f args)
-- default
        Ast n args
            -> let (r,b,c) = foldr (\a (r,b,c) -> let (x,s,i) = normalize a b c in (x:r,s,i))
                                   ([],changed,count) args
               in (Ast n r,b,c)
        _ -> (exp,changed,count)
      where --norm e = trace ("*** "++pp exp 4++"\n    "++pp e 4) (normalize e True count)
            norm e = normalize e True count
            pp (Ast _ _) 0 = "."
            pp (Ast nm (t:ts)) n = nm ++ "(" ++ pp t (n-1) ++ concatMap (\x -> "," ++ pp x (n-1)) ts ++ ")"
            pp x n = show x


foldSQL :: Ast -> Ast
foldSQL e
    = case e of
        Ast "for" [Avar v,Avar "$",Ast "call" [Avar "SQL",Ast "call" [Avar "select"],
                                               Ast "call" ((Avar "tables"):tables),pred],body]
            | any (\(Avar x) -> x==v) tables
            -> foldSQL (Ast "for" [Avar v,Avar "$",Ast "call" [Avar "SQL",Ast "call" [Avar "select",Avar (v++".*")],
                                                               Ast "call" ((Avar "tables"):tables),pred],body])
        Ast "call" [Avar "SQL",Ast "call" ((Avar "select"):cols),Ast "call" ((Avar "tables"):tables),pred]
            -> let (sql,args) = makeSQL [] tables pred cols
               in Ast "call" [Avar "sql",Astring sql,concatenateAll args]
        Ast n args -> Ast n (map foldSQL args)
        _ -> e


optimizeLoop :: Ast -> Int -> (Ast,Int)
optimizeLoop e c = let (ne,b,c') = normalize e False c
                   in if b
                      then optimizeLoop ne c'
                      else (ne,c)


optimize :: Ast -> Ast
optimize e = foldSQL (fst (optimizeLoop (simplify e) 0))
