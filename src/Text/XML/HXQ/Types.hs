{-------------------------------------------------------------------------------------
-
- The XQuery type system
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 01/16/09, last update: 01/16/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}


module Text.XML.HXQ.Types(readNum,toNum,toInt,toString,toFloat,
                          buildInTypes,isBuildInType,instanceOf,castAs,castableAs) where

import Char(isDigit)
import Text.XML.HXQ.Parser
import Text.XML.HXQ.XTree


buildInTypes :: [(String,String)]
buildInTypes
    = map (\(n,m) -> ("xs:"++n,"xs:"++m))
      [("untypedAtomic","anyAtomicType"),
       ("dateTime","anyAtomicType"),
       ("date","anyAtomicType"),
       ("time","anyAtomicType"),
       ("duration","anyAtomicType"),
       ("float","anyAtomicType"),
       ("double","anyAtomicType"),
       ("decimal","anyAtomicType"),
       ("gYearMonth","anyAtomicType"),
       ("gYear","anyAtomicType"),
       ("gMonthDay","anyAtomicType"),
       ("gDay","anyAtomicType"),
       ("gMonth","anyAtomicType"),
       ("boolean","anyAtomicType"),
       ("base64Binary","anyAtomicType"),
       ("hexBinary","anyAtomicType"),
       ("anyURI","anyAtomicType"),
       ("QName","anyAtomicType"),
       ("NOTATION","anyAtomicType"),
       ("yearMonthDuration","duration"),
       ("dayTmeDuration","duration"),
       ("Integer","decimal"),
       ("nonPositiveInteger","Integer"),
       ("negativeInteger","nonPositiveInteger"),
       ("long","Integer"),
       ("int","long"),
       ("short","int"),
       ("byte","short"),
       ("nonNegativeInteger","Integer"),
       ("unsignedLong","nonNegativeInteger"),
       ("unsignedInt","unsignedLong"),
       ("unsignedShort","unsignedInt"),
       ("unsignedByte","unsignedShort"),
       ("positiveInteger","nonNegativeInteger"),
       ("string","anyAtomicType"),
       ("normalizedString","string"),
       ("token","normalizedString"),
       ("language","token"),
       ("NMTOKEN","token"),
       ("Name","token"),
       ("NCName","Name"),
       ("ID","NCName"),
       ("IDREF","NCName"),
       ("ENTITY","NCName")]


isBuildInType :: String -> Bool
isBuildInType "xs:anyAtomicType" = True
isBuildInType name = memV name buildInTypes


-- find the value of a variable in an association list
findV var ((n,b):_) | n==var = b
findV var (_:xs) = findV var xs
findV var _ = error ("Undefined variable: "++var)


-- is the variable defined in the association list?
memV var ((n,_):_) | n==var = True
memV var (_:xs) = memV var xs
memV _ _ = False


-- xs:string casting. Much like string()
toString :: XTree -> String
toString x
    = case x of
        XElem _ _ _ _ zs
            -> concatMap toString zs
        XAttr _ v -> v
        XText x -> x
        XInt n -> show n
        XFloat n -> show n
        XBool b -> if b then "true" else "false"
        _ -> ""


-- parse a numeral (int or float) from a string
readNum :: String -> Maybe XTree
readNum cs
    = let readInt ('+':rest) = span isDigit rest
          readInt ('-':rest) = let (s,rest1) = span isDigit rest
                               in ('-':s,rest1)
          readInt rest = span isDigit rest
          readExp ('e':cs) = readInt cs
          readExp ('E':cs) = readInt cs
          readExp cs = ("",cs)
          (si,rest) = readInt cs
      in case rest of
           '.':rest1
               -> let (sd,rest2) = span isDigit rest1
                  in case readExp rest2 of
                       ("",[]) -> Just $ XFloat (read $ si ++ "." ++ sd)
                       (exp,[]) -> Just $ XFloat (read $ si ++ "." ++ sd ++ "e" ++ exp)
                       _ -> Nothing
           _ -> case readExp rest of
                  ("",[]) -> Just $ XInt (read si)
                  (exp,[]) -> Just $ XFloat (read $ si ++ "e" ++ exp)
                  _ -> Nothing


-- casting to any kind of numeral
toNum :: XTree -> Maybe XTree
toNum (XElem _ _ _ _ [x]) = toNum x
toNum (XText s) = readNum s
toNum (x@(XInt n)) = Just x
toNum (x@(XFloat n)) = Just x
toNum (XBool b) = Just $ XInt (if b then 1 else 0)
toNum (XAttr _ v) = toNum (XText v)
toNum _ = Nothing


-- xs:int casting
toInt :: XTree -> Maybe XTree
toInt (XElem _ _ _ _ [x]) = toInt x
toInt (XText s) = case readNum s of
                    Just (XFloat n) -> Just $ XInt (floor n)
                    x -> x
toInt (x@(XInt n)) = Just x
toInt (XFloat n) = Just $ XInt (floor n)
toInt (XBool b) = Just $ XInt (if b then 1 else 0)
toInt (XAttr _ v) = toInt (XText v)
toInt _ = Nothing


-- xs:float casting
toFloat :: XTree -> Maybe XTree
toFloat (XElem _ _ _ _ [x]) = toFloat x
toFloat (XText s) = case readNum s of
                      Just (XInt n) -> Just $ XFloat $ fromIntegral n
                      x -> x
toFloat (XInt n) = Just $ XFloat $ fromIntegral n
toFloat (x@(XFloat n)) = Just x
toFloat (XBool b) = Just $ XFloat (if b then 1 else 0)
toFloat (XAttr _ v) = toFloat (XText v)
toFloat _ = Nothing


-- xs:boolean casting
toBool :: XTree -> Maybe XTree
toBool (XText s) = Just $ XBool $ s /= ""
toBool (XInt n) = Just $ XBool $ n /= 0
toBool (XFloat n) = Just $ XBool $ n /= 0
toBool (XAttr _ v) = Just $ XBool $ v /= ""
toBool (XBool b) = Just $ XBool b
toBool x = Nothing


-- all casting functions
casts :: [(String,XTree->Maybe XTree)]
casts
    = map (\(n,f) -> ("xs:"++n,f))
      [("anyAtomicType",Just . id),
       ("string",Just . XText . toString),
       ("float",toFloat),
       ("Integer",toInt),
       ("nonNegativeInteger",\x -> do XInt n <- toInt x
                                      return $! XInt $ abs n),
       ("boolean",toBool)]


-- implements: expr instance of type
instanceOf :: XSeq -> Ast -> Bool
instanceOf expr typ
    = instOf expr typ
      where instOf [] (Ast "empty-sequence" []) = True
            instOf [] (Ast "?" _) = True
            instOf [x] (Ast "?" [tp]) = instOfOne x tp
            instOf xs (Ast "*" [tp])
                = all (\x -> instOfOne x tp) xs
            instOf xs (Ast "+" [tp])
                = (not $ null xs) && all (\x -> instOfOne x tp) xs
            instOf [x] tp = instOfOne x tp
            instOf _ _ = False
            instOfOne (XElem t _ _ _ xs) seqType
                = case seqType of
                    Ast "item" [] -> True
                    Ast "node" [] -> True
                    Ast "element" [] -> True
                    Ast "element" [Avar tag]
                        -> tag == "*" || t == tag
                    Ast "element" [Avar tag,Ast "?" [tp]]
                        -> (tag == "*" || t == tag)
                           && null xs || instOf xs tp
                    Ast "element" [Avar tag,tp]
                        -> (tag == "*" || t == tag) && instOf xs tp
                    _ -> False
            instOfOne (XAttr nm v) seqType
                = case seqType of
                    Ast "item" [] -> True
                    Ast "attribute" [Avar name]
                        -> name == "*" || nm == name
                    _ -> False
            instOfOne x (Avar tname)
                = if memV tname casts
                  then (findV tname casts x) /= Nothing
                  else if memV tname buildInTypes
                       then instOfOne x (Avar $ findV tname buildInTypes)
                       else error $ "Unrecognized build-in type: "++tname
            instOfOne _ _ = False


-- implements: expr cast as type
castAs :: XSeq -> Ast -> XSeq
castAs [] (Ast "?" _) = []
castAs [XElem _ _ _ _ xs] tp = castAs xs tp
castAs [x] (Avar tname)
    = if memV tname casts
      then case findV tname casts x of
             Just v -> [v]
             Nothing -> error $ "Value "++show x++" cannot be cast to the atomic type: "++tname
      else if memV tname buildInTypes
           then castAs [x] (Avar $ findV tname buildInTypes)
           else error $ "Unrecognized build-in type: "++tname
castAs xs tp = error $ "Value "++show xs++" cannot be cast to the type "++show tp


-- implements: expr castable as type
castableAs :: XSeq -> Ast -> Bool
castableAs [] (Ast "?" _) = True
castableAs [XElem _ _ _ _ xs] tp = castableAs xs tp
castableAs [x] (Avar tname)
    = if memV tname casts
      then case findV tname casts x of
             Just _ -> True
             Nothing -> False
      else if memV tname buildInTypes
           then castableAs [x] (Avar $ findV tname buildInTypes)
           else error $ "Unrecognized build-in type: "++tname
castableAs _ _ = False
