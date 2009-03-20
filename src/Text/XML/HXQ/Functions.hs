{-------------------------------------------------------------------------------------
-
- XQuery functions
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 08/15/08, last update: 01/17/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}


module Text.XML.HXQ.Functions where

import HXML(AttList)
import Data.List
import Char
import Text.XML.HXQ.XTree
import Text.XML.HXQ.Types
import Language.Haskell.TH
import Text.Regex
import Text.Regex.Base.RegexLike
import qualified GHC.Arr as A
import Debug.Trace


{--------------- XPath Steps ---------------------------------------------------------}


-- XPath step self /.
self_step :: String -> XTree -> XSeq
self_step tag x
    = case x of
        XElem t _ _ _ _ -> if t==tag || tag=="*" then [x] else []
        _ -> [x]


-- XPath step /tag or / *
child_step :: String -> XTree -> XSeq
child_step tag x
    = case x of
        XElem _ _ _ _ bs
            -> foldr (\b s -> case b of
                                 XElem t _ _ _ _ | (t==tag || tag=="*") -> b:s
                                 _ -> s) [] bs
        _ -> []


-- XPath step descendant-or-self
descendant_or_self_step :: String -> XTree -> XSeq
descendant_or_self_step tag (x@(XElem t _ _ _ cs))
    | tag==t || tag=="*"
    = x:(concatMap (descendant_or_self_step tag) cs)
descendant_or_self_step tag (XElem t _ _ _ cs)
    = concatMap (descendant_or_self_step tag) cs
descendant_or_self_step _ _ = []


-- XPath step //tag or // *
descendant_step :: String -> XTree -> XSeq
descendant_step tag (XElem t _ _ _ cs)
    = concatMap (descendant_or_self_step tag) cs
descendant_step _ _ = []


-- It's like //* but has tagged children, which are derived statically
-- After examining 100 children it gives up: this avoids space leaks
descendant_any_with_tagged_children :: [String] -> XTree -> XSeq
descendant_any_with_tagged_children tags (XElem t _ _ _ cs)
    = concatMap (ca tags) cs
      where ca tags (x@(XElem t _ _ _ cs))
                | all (\tag -> foldr (\b s -> case b of
                                                (XElem k _ _ _ _) -> s || k == tag
                                                _ -> s) False cs100) tags
                = x:(concatMap (ca tags) cs)
                where cs100 = take 100 cs
            ca tags (XElem t _ _ _ cs)
                = concatMap (ca tags) cs
            ca tags _ = []
descendant_any_with_tagged_children tags _ = []


-- XPath step /@attr or /@*
attribute_step :: String -> XTree -> XSeq
attribute_step attr x
    = case x of
        XElem _ al _ _ _ -> foldr (\(a,v) s -> if a==attr || attr=="*"
                                               then (XAttr a v):s
                                               else s) [] al
        _ -> []


-- XPath step //@attr or //@*
attribute_descendant_step :: String -> XTree -> XSeq
attribute_descendant_step attr (x@(XElem _ al _ _ cs))
    = foldr (\(a,v) s -> if a==attr || attr=="*"
                         then (XAttr a v):s
                         else s)
            (concatMap (attribute_descendant_step attr) cs) al
attribute_descendant_step _ _ = []


-- XPath step parent /..
parent_step :: String -> XTree -> XSeq
parent_step tag (XElem _ _ _ p _)
 = case p of
     XElem t _ _ _ _ | (t==tag || tag=="*") -> [p]
     _ -> []
parent_step _ _ = []


-- XPath step ancestor
ancestor_step :: String -> XTree -> XSeq
ancestor_step tag (XElem _ _ _ p _)
 = case p of
     XElem t _ _ _ _
         -> if t==tag || tag=="*"
            then p:(ancestor_step tag p)
            else ancestor_step tag p
     _ -> []
ancestor_step _ _ = []


-- XPath step ancestor-or-self
ancestor_or_self_step :: String -> XTree -> XSeq
ancestor_or_self_step tag e
 = case e of
     XElem t _ _ _ _
         -> if t==tag || tag=="*"
            then e:(ancestor_step tag e)
            else ancestor_step tag e
     _ -> []


-- XPath step following-sibling
following_sibling_step :: String -> XTree -> XSeq
following_sibling_step tag (XElem _ _ order (XElem _ _ _ _ cs) _)
 = concatMap (self_step tag)
             (tail (dropWhile filter cs))
   where filter (XElem _ _ o _ _) = o /= order
         filter _ = True
following_sibling_step _ _ = []


-- XPath step following
following_step :: String -> XTree -> XSeq
following_step tag (XElem _ _ order p _)
 = case p of
     XElem _ _ _ _ cs
         -> (concatMap (descendant_or_self_step tag)
                       (tail (dropWhile filter cs)))
            ++(following_step tag p)
            where filter (XElem _ _ o _ _) = o /= order
                  filter _ = True
     _ -> []
following_step _ _ = []


-- XPath step preceding-sibling
preceding_sibling_step :: String -> XTree -> XSeq
preceding_sibling_step tag (XElem _ _ order (XElem _ _ _ _ cs) _)
 = concatMap (self_step tag)
             (takeWhile filter cs)
   where filter (XElem _ _ o _ _) = o /= order
         filter _ = True
preceding_sibling_step _ _ = []


-- XPath step preceding
preceding_step :: String -> XTree -> XSeq
preceding_step tag (XElem _ _ order p _)
 = case p of
     XElem t _ _ _ cs
         -> (concatMap (descendant_or_self_step tag)
                       (takeWhile filter cs))
            ++(preceding_step tag p)
            where filter (XElem _ _ o _ _) = o /= order
                  filter _ = True
     _ -> []
preceding_step _ _ = []


-- XPath steps
paths :: [(String,Q Exp)]
paths = [ ( "child", [| child_step |] ),
          ( "descendant", [| descendant_step |] ),
          ( "attribute", [| attribute_step |] ),
          ( "self", [| self_step |] ),
          ( "descendant-or-self", [| descendant_or_self_step |] ),
          ( "attribute-descendant", [| attribute_descendant_step |] ),
          ( "following-sibling", [| following_sibling_step |] ),
          ( "following", [| following_step |] ),
          ( "parent", [| parent_step |] ),
          ( "ancestor", [| ancestor_step |] ),
          ( "preceding-sibling", [| preceding_sibling_step |] ),
          ( "preceding", [| preceding_step |] ),
          ( "ancestor-or-self", [| ancestor_or_self_step |] ) ]


-- XPath steps to be used by the interpreter
pathFunctions :: [(String,String->XTree->XSeq)]
pathFunctions
    = [ ( "child", child_step ),
        ( "descendant", descendant_step ),
        ( "attribute", attribute_step ),
        ( "self", self_step ),
        ( "descendant-or-self", descendant_or_self_step ),
        ( "attribute-descendant", attribute_descendant_step ),
        ( "following-sibling", following_sibling_step ),
        ( "following", following_step ),
        ( "parent", parent_step ),
        ( "ancestor", ancestor_step ),
        ( "preceding-sibling", preceding_sibling_step ),
        ( "preceding", preceding_step ),
        ( "ancestor-or-self", ancestor_or_self_step ) ]


{------------ Functions --------------------------------------------------------------}


-- find the value of a variable in an association list
findV var ((n,b):_) | n==var = b
findV var (_:xs) = findV var xs
findV var _ = error ("Undefined variable: "++var)


-- is the variable defined in the association list?
memV var ((n,_):_) | n==var = True
memV var (_:xs) = memV var xs
memV _ _ = False


-- lazy: like foldr but with an index
foldir :: (a -> Int -> b -> b) -> b -> [a] -> Int -> b
foldir c n [] i = n
foldir c n (x:xs) i = c x i (foldir c n xs $! (i+1))


-- strict: like foldl' but with an index
foldil :: (a -> Int -> b -> b) -> b -> [a] -> Int -> b
foldil c n [] i = n
foldil c n (x:xs) i = (foldil c $! (c x i n)) xs $! (i+1)


trueXT = XBool True
falseXT = XBool False


toBool :: Bool -> XSeq
toBool b = if b then [trueXT] else [falseXT]


text :: XSeq -> XSeq
text xs = foldr (\x r -> case x of
                            XElem _ _ _ _ zs
                                -> (filter (\a -> case a of XText _ -> True; XInt _ -> True; XNull -> True;
                                                            XFloat _ -> True; XBool _ -> True; _ -> False) zs)++r
                            XText _ -> x:r
                            XInt _ -> x:r
                            XFloat _ -> x:r
                            XBool _ -> x:r
                            _ -> r) [] xs


string :: XSeq -> XSeq
string xs = foldr (\x r -> case x of
                             XElem _ _ _ _ zs -> (string zs)++r
                             XAttr _ v -> (XText v):r
                             XText _ -> x:r
                             XInt _ -> x:r
                             XFloat _ -> x:r
                             XBool _ -> x:r
                             _ -> r) [] xs


strings :: XSeq -> [String]
strings xs = map toString xs


getNames :: XSeq -> XSeq
getNames xs = foldr (\x r -> case x of 
                               XElem tag _ _ _ _ -> (XText tag):r
                               XAttr tag _ -> (XText tag):r
                               _ -> r) [] xs


-- concatenate text with no padding (for element content)
appendText :: [XSeq] -> XSeq
appendText [] = []
appendText [x] = x
appendText (x:xs) = x++(XNoPad:(appendText xs))


substring_before :: String -> String -> String
substring_before xs ys
    = s xs ys []
      where s xs ys c | isPrefixOf ys xs = c
            s (x:xs) ys c = s xs ys (c++[x])
            s _ _ _ = []


substring_after :: String -> String -> String
substring_after xs ys
    = s xs ys
      where s xs ys | isPrefixOf ys xs = drop (length ys) xs
            s (_:xs) ys = s xs ys
            s _ _ = []


replaceString :: String -> String -> [(Int,Int)] -> String
replaceString from with indexes
    = rs from indexes 0 ""
      where rs "" _ _ s = s
            rs fs [] _ s = s++fs
            rs fs ((i,l):is) j s
                | i==j
                = rs (drop l fs) is (j+l) (s++with)
            rs (f:fs) is j s
                = rs fs is (j+1) (s++[f])


translate_string :: String -> String -> String -> String
translate_string xs from to
    = foldr (\c r -> case elemIndex c from of
                       Just i -> if i < length to
                                 then (to !! i):r
                                 else r
                       _ -> c:r) "" xs


toNums :: XSeq -> XSeq
toNums xs = case mapM toNum xs of Just x -> x; _ -> []


getFloat :: XTree -> Double
getFloat x = case toFloat x of
               Just (XFloat n) -> n
               _ -> error("Cannot convert to a double: "++show x)


-- strict: average value
mean :: [Double] -> Double
mean = uncurry (/) . foldl' (\(!s, !n) x -> (s+x, n+1)) (0,0.0)


sumSeq :: XSeq -> XSeq
sumSeq xs
    = case xs of
        XInt _:_ -> [ XInt (sum [ n | XInt n <- xs ]) ]
        XFloat _:_ -> [ XFloat (sum [ n | XFloat n <- xs ]) ]
        _ -> []


maxSeq :: XSeq -> XSeq
maxSeq [] = []
maxSeq xs = [ maximumBy compareXTrees xs ]


minSeq :: XSeq -> XSeq
minSeq [] = []
minSeq xs = [ minimumBy compareXTrees xs ]


contains :: String -> String -> Bool
contains text word = isInfixOf word text


-- lazy: remove duplicates
distinct :: Eq a => [a] -> [a]
distinct xs
    = rd xs []
      where rd (x:xs) r = if elem x r then rd xs r else x:(rd xs $! (x:r))
            rd [] _ = []


modulo :: Double -> Double -> Double
modulo x y = x - fromIntegral(floor(x/y))*y


arithmetic :: (Double -> Double -> Double) -> XTree -> XTree -> XTree
arithmetic op (XInt n) (XInt m) = XInt (floor (op (fromIntegral n) (fromIntegral m)))
arithmetic op (XFloat n) (XFloat m) = XFloat (op n m)
arithmetic op (XFloat n) (XInt m) = XFloat (op n (fromIntegral m))
arithmetic op (XInt n) (XFloat m) = XFloat (op (fromIntegral n) m)


compareXTrees :: XTree -> XTree -> Ordering
compareXTrees (XElem _ _ _ _ _) _ = EQ
compareXTrees _ (XElem _ _ _ _ _) = EQ
compareXTrees (XInt n) (XInt m) = compare n m
compareXTrees (XFloat n) (XInt m) = compare n (fromIntegral m)
compareXTrees (XInt n) (XFloat m) = compare (fromIntegral n) m
compareXTrees (XFloat n) (XFloat m) = compare n m
compareXTrees (XText n) (XText m) = compare n m
compareXTrees (XBool n) (XBool m) = compare n m
compareXTrees x y = compare (toString x) (toString y)


strictCompareOne [XInt n] [XInt m] = compare n m
strictCompareOne [XFloat n] [XFloat m] = compare n m
strictCompareOne [XFloat n] [XInt m] = compare n (fromIntegral m)
strictCompareOne [XInt n] [XFloat m] = compare (fromIntegral n) m
strictCompareOne [XText n] [XText m] = compare n m
strictCompareOne [XBool n] [XBool m] = compare n m
strictCompareOne x y = error ("Illegal operands in strict comparison: "++(show x)++" "++(show y))

strictCompare :: XSeq -> XSeq -> Ordering
strictCompare [XElem _ _ _ _ x] [XElem _ _ _ _ y] = strictCompareOne x y
strictCompare x [XElem _ _ _ _ y] = strictCompareOne x y
strictCompare [XElem _ _ _ _ x] y = strictCompareOne x y
strictCompare x y = strictCompareOne x y


compareXSeqs :: Bool -> XSeq -> XSeq -> Ordering
compareXSeqs ord xs ys
    = let comps = [ compareXTrees x y | x <- xs, y <- ys ]
      in if ord
            then if all (\x -> x == LT) comps
                    then LT
                 else if all (\x -> x == GT) comps
                    then GT
                 else EQ
         else if all (\x -> x == LT) comps
                 then GT
              else if all (\x -> x == GT) comps
                 then LT
              else EQ


deep_equal :: XTree -> XTree -> Bool
deep_equal (XElem t1 a1 _ _ xs1) (XElem t2 a2 _ _ xs2)
    = t1 == t2 && sort a1 == a2 && (and $ zipWith deep_equal xs1 xs2)
deep_equal (XElem _ _ _ _ _) _ = False
deep_equal _ (XElem _ _ _ _ _) = False
deep_equal x y = x == y


conditionTest :: XSeq -> Bool
conditionTest [] = False
conditionTest [XText ""] = False
conditionTest [XInt 0] = False
conditionTest [XBool False] = False
conditionTest _ = True


-- lazy indexing (similar to !!)
index :: [a] -> Int -> [a]
index [] n = []
index (x:xs) 0 = [x]
index (_:xs) n = index xs $! (n-1)


system_functions :: [String]
system_functions = map (\(nm,_,_,_) -> nm) systemFunctions ++ map (\(nm,_,_,_) -> "fn:"++nm) systemFunctions


-- System functions
systemFunctions :: [(String,                -- function name
                     Int,                   -- arity (-1 means any number of arguments)
                     [XSeq]->XSeq,          -- function to be used by the interpreter
                     [Q Exp] -> Q Exp)]     -- function to be used by the compiler
systemFunctions
    = [
--  general comparisons
        ( "=", 2,
          \[xs,ys] -> toBool $ or [ compareXTrees x y == EQ | x <- text xs, y <- text ys ],
          \[xs,ys] -> [| toBool $ or [ compareXTrees x y == EQ | x <- text $xs, y <- text $ys ] |] ),
        ( "!=", 2,
          \[xs,ys] -> toBool $ strictCompare xs ys /= EQ,
          \[xs,ys] -> [| toBool $ strictCompare $xs $ys /= EQ |] ),
        ( ">", 2,
          \[xs,ys] -> toBool $ or [ compareXTrees x y == GT | x <- text xs, y <- text ys ],
          \[xs,ys] -> [| toBool $ or [ compareXTrees x y == GT | x <- text $xs, y <- text $ys ] |] ),
        ( "<", 2,
          \[xs,ys] -> toBool $ or [ compareXTrees x y == LT | x <- text xs, y <- text ys ],
          \[xs,ys] -> [| toBool $ or [ compareXTrees x y == LT | x <- text $xs, y <- text $ys ] |] ),
        ( ">=", 2,
          \[xs,ys] -> toBool $ or [ compareXTrees x y `elem` [GT,EQ] | x <- text xs, y <- text ys ],
          \[xs,ys] -> [| toBool $ or [ compareXTrees x y `elem` [GT,EQ] | x <- text $xs, y <- text $ys ] |] ),
        ( "<=", 2,
          \[xs,ys] -> toBool $ or [ compareXTrees x y `elem` [LT,EQ] | x <- text xs, y <- text ys ],
          \[xs,ys] -> [| toBool $ or [ compareXTrees x y `elem` [LT,EQ] | x <- text $xs, y <- text $ys ] |] ),
        ( "deep-equal", 2,
          \[xs,ys] -> toBool $ or [ deep_equal x y | x <- xs, y <- ys ],
          \[xs,ys] -> [| toBool $ or [ deep_equal x y | x <- $xs, y <- $ys ] |] ),
--  strict comparisons
        ( "eq", 2,
          \[xs,ys] -> toBool $ strictCompare xs ys == EQ,
          \[xs,ys] -> [| toBool $ strictCompare $xs $ys == EQ |] ),
        ( "neq", 2,
          \[xs,ys] -> toBool $ strictCompare xs ys /= EQ,
          \[xs,ys] -> [| toBool $ strictCompare $xs $ys /= EQ |] ),
        ( "lt", 2,
          \[xs,ys] -> toBool $ strictCompare xs ys == LT,
          \[xs,ys] -> [| toBool $ strictCompare $xs $ys == LT |] ),
        ( "gt", 2,
          \[xs,ys] -> toBool $ strictCompare xs ys == GT,
          \[xs,ys] -> [| toBool $ strictCompare $xs $ys == GT |] ),
        ( "le", 2,
          \[xs,ys] -> toBool $ strictCompare xs ys `elem` [LT,EQ],
          \[xs,ys] -> [| toBool $ strictCompare $xs $ys `elem` [LT,EQ] |] ),
        ( "ge", 2,
          \[xs,ys] -> toBool $ strictCompare xs ys `elem` [GT,EQ],
          \[xs,ys] -> [| toBool $ strictCompare $xs $ys `elem` [GT,EQ] |] ),
-- document order comparisons
        ( "<<", 2,
          \[xs,ys] -> toBool $ or [ ox < oy | XElem _ _ ox _ _ <- xs, XElem _ _ oy _ _ <- ys ],
          \[xs,ys] -> [| toBool $ or [ ox < oy | XElem _ _ ox _ _ <- $xs, XElem _ _ oy _ _ <- $ys ] |] ),
        ( ">>", 2,
          \[xs,ys] -> toBool $ or [ ox > oy | XElem _ _ ox _ _ <- xs, XElem _ _ oy _ _ <- ys ],
          \[xs,ys] -> [| toBool $ or [ ox > oy | XElem _ _ ox _ _ <- $xs, XElem _ _ oy _ _ <- $ys ] |] ),
        ( "is", 2,
          \[xs,ys] -> toBool $ or [ ox == oy | XElem _ _ ox _ _ <- xs, XElem _ _ oy _ _ <- ys ],
          \[xs,ys] -> [| toBool $ or [ ox == oy | XElem _ _ ox _ _ <- $xs, XElem _ _ oy _ _ <- $ys ] |] ),
-- arithmetic operations
        ( "+", 2,
          \[xs,ys] -> [ arithmetic (+) x y | x <- toNums xs, y <- toNums ys ],
          \[xs,ys] -> [| [ arithmetic (+) x y | x <- toNums $xs, y <- toNums $ys ] |] ),
        ( "-", 2,
          \[xs,ys] -> [ arithmetic (-) x y | x <- toNums xs, y <- toNums ys ],
          \[xs,ys] -> [| [ arithmetic (-) x y | x <- toNums $xs, y <- toNums $ys ] |] ),
        ( "*", 2,
          \[xs,ys] -> [ arithmetic (*) x y | x <- toNums xs, y <- toNums ys ],
          \[xs,ys] -> [| [ arithmetic (*) x y | x <- toNums $xs, y <- toNums $ys ] |] ),
        ( "div", 2,
          \[xs,ys] -> [ arithmetic (/) x y | x <- toNums xs, y <- toNums ys ],
          \[xs,ys] -> [| [ arithmetic (/) x y | x <- toNums $xs, y <- toNums $ys ] |] ),
        ( "idiv", 2,
          \[xs,ys] -> [ case arithmetic (/) x y of XFloat n -> XInt (floor n); x -> x
                      | x <- toNums xs, y <- toNums ys ],
          \[xs,ys] -> [| [ case arithmetic (/) x y of XFloat n -> XInt (floor n); x -> x
                         | x <- toNums $xs, y <- toNums $ys ] |] ),
        ( "mod", 2,
          \[xs,ys] -> [ arithmetic modulo x y | x <- toNums xs, y <- toNums ys ],
          \[xs,ys] -> [| [ arithmetic modulo x y | x <- toNums $xs, y <- toNums $ys ] |] ),
        ( "uplus", 1,
          \[xs] -> [ x | x <- toNums xs ],
          \[xs] -> [| [ x | x <- toNums $xs ] |] ),
        ( "uminus", 1,
          \[xs] -> [ case x of XInt n -> XInt (-n); XFloat n -> XFloat (-n) | x <- toNums xs ],
          \[xs] -> [| [ case x of XInt n -> XInt (-n); XFloat n -> XFloat (-n) | x <- toNums $xs ] |] ),
        ( "abs", 1,
          \[xs] -> [ case x of XInt n -> XInt (abs n); XFloat n -> XFloat (abs n) | x <- toNums xs ],
          \[xs] -> [| [ case x of XInt n -> XInt (abs n); XFloat n -> XFloat (abs n) | x <- toNums $xs ] |] ),
        ( "ceiling", 1,
          \[xs] -> [ case x of XInt n -> XInt n; XFloat n -> XInt (ceiling n) | x <- toNums xs ],
          \[xs] -> [| [ case x of XInt n -> XInt n; XFloat n -> XInt (ceiling n) | x <- toNums $xs ] |] ),
        ( "round", 1,
          \[xs] -> [ case x of XInt n -> XInt n; XFloat n -> XInt (round n) | x <- toNums xs ],
          \[xs] -> [| [ case x of XInt n -> XInt n; XFloat n -> XInt (round n) | x <- toNums $xs ] |] ),
        ( "floor", 1,
          \[xs] -> [ case x of XInt n -> XInt n; XFloat n -> XInt (floor n) | x <- toNums xs ],
          \[xs] -> [| [ case x of XInt n -> XInt n; XFloat n -> XInt (floor n) | x <- toNums $xs ] |] ),
-- boolean operations
        ( "true", 0,
          \[] -> [trueXT],
          \[] -> [| [trueXT] |] ),
        ( "false", 0,
          \[] -> [falseXT],
          \[] -> [| [falseXT] |] ),
        ( "if", 3,
          \[cs,xs,ys] -> if conditionTest cs then xs else ys,
          \[cs,xs,ys] -> [| if conditionTest $cs then $xs else $ys |] ),
        ( "and", 2,
          \[xs,ys] -> toBool $ (conditionTest xs) && (conditionTest ys),
          \[xs,ys] -> [| toBool $ (conditionTest $xs) && (conditionTest $ys) |] ),
        ( "or", 2,
          \[xs,ys] -> toBool $ (conditionTest xs) || (conditionTest ys),
          \[xs,ys] -> [| toBool $ (conditionTest $xs) || (conditionTest $ys) |] ),
        ( "not", 1,
          \[xs] -> toBool $ not $ conditionTest xs,
          \[xs] -> [| toBool $ not $ conditionTest $xs |] ),
        ( "some", 1,
          \[xs] -> toBool $ or [ conditionTest [x] | x <- xs ],
          \[xs] -> [| toBool $ or [ conditionTest [x] | x <- $xs ] |] ),
-- aggregations
        ( "count", 1,
          \[xs] -> [ XInt (length xs) ],
          \[xs] -> [| [ XInt (length $xs) ] |] ),
        ( "sum", 1,
          \[xs] -> sumSeq xs,
          \[xs] -> [| sumSeq $xs |] ),
        ( "avg", 1,
          \[xs] -> [ XFloat (mean [ getFloat x | x <- toNums xs ]) ],
          \[xs] -> [| [ XFloat (mean [ getFloat x | x <- toNums $xs ]) ] |] ),
        ( "min", 1,
          \[xs] -> minSeq xs,
          \[xs] -> [| minSeq $xs |] ),
        ( "max", 1,
          \[xs] -> maxSeq xs,
          \[xs] -> [| maxSeq $xs |] ),
-- string operations
        ( "concat", -1,
          \ss -> [ XText $ foldr (\s r -> concat [ x | x <- strings s ] ++ r) "" ss ],
          \ss -> [| [ XText $ $(foldr (\s r -> [| concat [ x | x <- strings $s ] ++ $r |]) [| "" |] ss) ] |] ),
        ( "substring", 3,
          \[xs,n1,n2] -> [ XText (take m2 (drop (m1-1) x))
                         | x <- strings xs, XInt m1 <- toNums n1, XInt m2 <- toNums n2 ],
          \[xs,n1,n2] -> [| [ XText (take m2 (drop (m1-1) x))
                            | x <- strings $xs, XInt m1 <- toNums $n1, XInt m2 <- toNums $n2 ] |] ),
        ( "substring", 2,
          \[xs,n] -> [ XText (drop (m-1) x) | x <- strings xs, XInt m <- toNums n ],
          \[xs,n] -> [| [ XText (drop (m-1) x) | x <- strings $xs, XInt m <- toNums $n ] |] ),
        ( "substring-before", 2,
          \[xs,ys] -> [ XText (substring_before x y)  | x <- strings xs, y <- strings ys ],
          \[xs,ys] -> [| [ XText (substring_before x y)  | x <- strings $xs, y <- strings $ys ] |] ),
        ( "substring-after", 2,
          \[xs,ys] -> [ XText (substring_after x y)  | x <- strings xs, y <- strings ys ],
          \[xs,ys] -> [| [ XText (substring_after x y)  | x <- strings $xs, y <- strings $ys ] |] ),
        ( "starts-with", 2,
          \[xs,ys] -> toBool $ or [ x == "" || isPrefixOf y x  | x <- strings xs, y <- strings ys ],
          \[xs,ys] -> [| toBool $ or [ x == "" || isPrefixOf y x  | x <- strings $xs, y <- strings $ys ] |] ),
        ( "ends-with", 2,
          \[xs,ys] -> toBool $ or [ x == "" || isSuffixOf y x  | x <- strings xs, y <- strings ys ],
          \[xs,ys] -> [| toBool $ or [ x == "" || isSuffixOf y x  | x <- strings $xs, y <- strings $ys ] |] ),
        ( "string-join", 2,
          \[xs,ys] -> [ XText $ intercalate y (strings xs) | y <- strings ys ],
          \[xs,ys] -> [| [ XText $ intercalate y (strings $xs) | y <- strings $ys ] |] ),
        ( "string-length", 1,
          \[xs] -> [ XInt $ length x | x <- strings xs ],
          \[xs] -> [| [ XInt $ length x | x <- strings $xs ] |] ),
        ( "translate", 3,
          \[xs,ys,zs] -> [ XText $ translate_string x y z  | x <- strings xs, y <- strings ys, z <- strings zs ],
          \[xs,ys,zs] -> [| [ XText $ translate_string x y z  | x <- strings $xs, y <- strings $ys, z <- strings $zs ] |] ),
        ( "matches", 2,
          \[xs,ys] -> toBool $ or [ matchTest (mkRegex y) x | x <- strings xs, y <- strings ys ],
          \[xs,ys] -> [| toBool $ or [ matchTest (mkRegex y) x | x <- strings $xs, y <- strings $ys ] |] ),
        ( "replace", 3,
          \[xs,ys,zs] -> [ XText $ replaceString x z (map snd $ concatMap A.elems $ matchAllText (mkRegex y) x)
                         | x <- strings xs, y <- strings ys, z <- strings zs ],
          \[xs,ys,zs] -> [| [ XText $ replaceString x z (map snd $ concatMap A.elems $ matchAllText (mkRegex y) x)
                            | x <- strings $xs, y <- strings $ys, z <- strings $zs ] |] ),
        ( "contains", 2,
          \[xs,text] -> toBool $ or [ contains x t | x <- strings xs, t <- strings text ],
          \[xs,text] -> [| toBool $ or [ contains x t | x <- strings $xs, t <- strings $text ] |] ),
        ( "compare", 2,
          \[xs,ys] -> [ XInt $ case compare x y of EQ -> 0; LT -> -1; GT -> 1 | x <- strings xs, y <- strings ys ],
          \[xs,ys] -> [| [ XInt $ case compare x y of EQ -> 0; LT -> -1; GT -> 1 | x <- strings $xs, y <- strings $ys ] |] ),
        ( "upper-case", 1,
          \[xs] -> [ XText $ map toUpper x | x <- strings xs ],
          \[xs] -> [| [ XText $ map toUpper x | x <- strings $xs ] |] ),
        ( "lower-case", 1,
          \[xs] -> [ XText $ map toLower x | x <- strings xs ],
          \[xs] -> [| [ XText $ map toLower x | x <- strings $xs ] |] ),
        ( "normalize-space", 1,
          \[xs] -> [ XText $ unwords $ words x  | x <- strings xs ],
          \[xs] -> [| [ XText $ unwords $ words x  | x <- strings $xs ] |] ),
-- sequence operations
        ( "empty", 0,
          \[] -> [],
          \[] -> [| [] |] ),
        ( "empty", 1,
          \[xs] -> toBool $ null xs,
          \[xs] -> [| toBool $ null $xs |] ),
        ( "exists", 1,
          \[xs] -> toBool $ (not (null xs)),
          \[xs] -> [| toBool $ (not (null $xs)) |] ),
        ( "to", 2,
          \[xs,ys] -> [ XInt i | XInt n <- toNums xs, XInt m <- toNums ys, i <- [n..m] ],
          \[xs,ys] -> [| [ XInt i | XInt n <- toNums $xs, XInt m <- toNums $ys, i <- [n..m] ] |] ),
        ( "concatenate", 2,
          \[xs,ys] -> xs ++ ys,
          \[xs,ys] -> [| $xs ++ $ys |] ),
        ( "distinct-values", 1,
          \[xs] -> distinct xs,
          \[xs] -> [| distinct $xs |] ),
        ( "union", 2,
          \[xs,ys] -> distinct (xs ++ ys),
          \[xs,ys] -> [| distinct ($xs ++ $ys) |] ),
        ( "intersect", 2,
          \[xs,ys] -> intersect xs ys,
          \[xs,ys] -> [| intersect $xs $ys |] ),
        ( "except", 2,
          \[xs,ys] -> xs \\ ys,
          \[xs,ys] -> [| $xs \\ $ys |] ),
        ( "reverse", 1,
          \[xs] -> reverse xs,
          \[xs] -> [| reverse $xs |] ),
        ( "subsequence", 2,
          \[xs,n] -> concat [ drop (m-1) xs | XInt m <- toNums n ],
          \[xs,n] -> [| concat [ drop (m-1) $xs | XInt m <- toNums $n ] |] ),
        ( "subsequence", 3,
          \[xs,n1,n2] -> concat [ take m2 (drop (m1-1) xs) | XInt m1 <- toNums n1, XInt m2 <- toNums n2 ],
          \[xs,n1,n2] -> [| concat [ take m2 (drop (m1-1) $xs) | XInt m1 <- toNums $n1, XInt m2 <- toNums $n2 ] |] ),
        ( "insert-before", 3,
          \[xs,n,ys] -> concat [ (take (i-1) xs)++ys++(drop (i-1) xs) | XInt i <- toNums n ],
          \[xs,n,ys] -> [| let x = $xs in concat [ (take (i-1) x) ++ $ys ++ (drop (i-1) x) | XInt i <- toNums $n ] |] ),
        ( "index-of", 2,
          \[xs,ys] -> [ XInt (i+1) | y <- ys, i <- elemIndices y xs ],
          \[xs,ys] -> [| [ XInt (i+1) | y <- $ys, i <- elemIndices y $xs ] |] ),
        ( "remove", 2,
          \[xs,ys] -> concat [ (take (i-1) xs)++(drop i xs) | XInt i <- toNums ys ],
          \[xs,ys] -> [| let x = $xs in concat [ (take (i-1) x)++(drop i x) | XInt i <- toNums $ys ] |] ),
-- type testing and casting
        ( "text", 1,
          \[xs] -> [ w | XElem _ _ _ _ ts <- xs, w <- text ts ],
          \[xs] -> [| [ w | XElem _ _ _ _ ts <- $xs, w <- text ts ] |] ),
        ( "string", 1,
          \[xs] -> string xs,
          \[xs] -> [| string $xs |] ),
        ( "name", 1,
          \[xs] -> getNames xs,
          \[xs] -> [| getNames $xs |] ),
        ( "local-name", 1,
          \[xs] -> getNames xs,
          \[xs] -> [| getNames $xs |] ),
        ( "data", 1,
          \[xs] -> text xs,
          \[xs] -> [| text $xs |] ),
        ( "number", 1,
          \[xs] -> toNums xs,
          \[xs] -> [| toNums $xs |] ),
        ( "boolean", 1,
          \[xs] -> toBool $ or [ conditionTest [x] | x <- xs ],
          \[xs] -> [| toBool $ or [ conditionTest [x] | x <- $xs ] |] ),
        ( "node", 1,
          \[xs] -> [ w | XElem _ _ _ _ ts <- xs, w <- ts ],
          \[xs] -> [| [ w | XElem _ _ _ _ ts <- $xs, w <- ts ] |] ),
        ( "instance-of", 2,
          \[e,[XType tp]] -> [ XBool $ instanceOf e tp ],
          \[e,tp] -> [| case $tp of [XType t] -> [ XBool $ instanceOf $e t ] |] ),
        ( "cast-as", 2,
          \[e,[XType tp]] -> castAs e tp,
          \[e,tp] -> [| case $tp of [XType t] -> castAs $e t |] ),
        ( "castable-as", 2,
          \[e,[XType tp]] -> [ XBool $ castableAs e tp ],
          \[e,tp] -> [| case $tp of [XType t] -> [ XBool $ castableAs $e t ] |] ),
-- miscellaneous operations
        ( "trace", 2,
          \[xs,ys] -> trace ("*** trace: "++show xs) ys,
          \[xs,ys] -> [| trace ("*** trace: "++show $xs) $ys |] ),
        ( "debug", 1,
          \_ -> error "the debug() call must be handled separately",
          \_ -> error "Debugging is not permitted at compile-time." ),
        ( "error", 0,
          \[] -> error "XQuery error" ,
          \[] -> [| error "XQuery error" |] ),
        ( "error", 2,
          \[xs,ys] -> error (showXS xs++": "++show ys),
          \[xs,ys] -> [| error (showXS $xs++": "++show $ys) |] ),
        ( "last", 0,
          \[] -> error "the 'last()' call must be handled separately",
          \[] -> error "the 'last()' call must be handled separately" ),
        ( "position", 0,
          \[] -> error "the 'position()' call must be handled separately",
          \[] -> error "the 'position()' call must be handled separately" )
   ]
