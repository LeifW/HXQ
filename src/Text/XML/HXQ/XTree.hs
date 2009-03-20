{-------------------------------------------------------------------------------------
-
- XML Trees (represented as rose trees)
- Programmer: Leonidas Fegaras
- Email: fegaras@cse.uta.edu
- Web: http://lambda.uta.edu/
- Creation: 05/01/08, last update: 01/17/09
- 
- Copyright (c) 2008 by Leonidas Fegaras, the University of Texas at Arlington. All rights reserved.
- This material is provided as is, with absolutely no warranty expressed or implied.
- Any use is at your own risk. Permission is hereby granted to use or copy this program
- for any purpose, provided the above notices are retained on all copies.
-
--------------------------------------------------------------------------------------}


{-# OPTIONS_GHC -funbox-strict-fields #-}


module Text.XML.HXQ.XTree
    ( XTree(..), XSeq, materialize, putXSeq, showXS ) where

import System.IO
import Char(isSpace)
import XMLParse(XMLEvent(..))
import HXML(Name,AttList)
import Text.XML.HXQ.Parser(Ast(..))


-- | A rose tree representation of XML data.
-- An XML element is:  @XElem tagname atributes preorder parent children@.
-- The preorder numbering is the document order of elements.
-- The parent is a cyclic reference to the parent element.
data XTree =  XElem    !Name !AttList !Int XTree [XTree]   -- ^ an XML tree node (element)
           |  XAttr    !Name !String    -- ^ attribute construction
           |  XText    !String          -- ^ an XML tree leaf (PCDATA)
           |  XInt     !Int             -- ^ an XML tree leaf (int)
           |  XFloat   !Double          -- ^ an XML tree leaf (double)
           |  XBool    !Bool            -- ^ an XML tree leaf (boolean)
           |  XPI      Name String	-- ^ processing instruction
           |  XGERef   String		-- ^ general entity reference
           |  XComment String		-- ^ comment
           |  XError   String		-- ^ error report
           |  XNull                     -- ^ null value
           |  XType    Ast              -- ^ type information
           |  XNoPad                    -- ^ marker for no padding in XSeq
           deriving Eq


-- | A sequence of XML fragments
type XSeq = [XTree]


emptyElem :: XTree -> Bool
emptyElem e
    = case e of
        XElem _ al _ _ xs
              -> emptyAL al && all emptyElem xs && not (null xs)
        XText text -> all isSpace text
        XNull -> True
        XNoPad -> True
        _ -> False
      where emptyAL = all (\(a,v) -> case (a,v) of (_,"") -> True; ('_':_,_) -> True; _ -> False)


showAL :: AttList -> String
showAL = foldr (\(a,v) r -> case (a,v) of ('_':_,_) -> r; _ -> " "++a++"=\""++v++"\""++r) []

showXT :: XTree -> Bool -> String
showXT e pad
    = case e of
        XElem "_document" _ _ _ xs -> showXS xs
        XElem tag al _ _ [] -> "<"++tag++showAL al++"/>"
        XElem _ _ _ _ _ | emptyElem e -> ""
        XElem tag al _ _ xs | all emptyElem xs -> "<"++tag++showAL al++"/>"
        XElem tag al _ _ xs -> "<"++tag++showAL al++">"++showXS xs++"</"++tag++">"
        XAttr tag val -> p++tag++"=\""++val++"\""
        XText text -> p ++ textEscape text
        XInt n -> p++show n
        XFloat n -> p++show n
        XBool v -> p++if v then "true" else "false"
        XComment s -> "<!--"++s++"-->"
        XPI n s -> "<?"++n++" "++s++">"
        XGERef s -> '&':s++";"		-- ^ general entity reference
        XError s -> error s
        XNull -> "?"
        XType tp -> show tp
        _ -> ""
      where p = if pad then " " else ""

showXS :: XSeq -> String
showXS [] = ""
showXS (x:xs) = showXT x False ++ sXS xs
    where sXS (XNoPad:x:xs) = (showXT x False) ++ sXS xs
          sXS (x:xs) = (showXT x True) ++ sXS xs
          sXS _ = ""

instance Show XTree where
    show t = showXT t False

textEscape []  = []
textEscape (c:cs)  =
    case c of
	'<'	-> "&lt;" ++ textEscape cs 
	'>'	-> "&gt;" ++ textEscape cs 
	'&'	-> "&amp;" ++ textEscape cs 
	_	-> c : textEscape cs 


-- | Print the XQuery result (which is a sequence of XML fragments) without buffering.
putXSeq :: XSeq -> IO ()
putXSeq xs = hSetBuffering stdout NoBuffering >> putStrLn (showXS xs)


{--------------- Build the rose tree from the XML stream ----------------------------}


type Stream = [XMLEvent]

noParentError = XError "Undefined parent reference"


-- Lazily materialize the SAX stream into a DOM tree without setting parent references.
materializeWithoutParent :: Stream -> XTree
materializeWithoutParent stream
    = XElem "_document" [] 1 noParentError
            [head (filter (\x -> case x of XElem _ _ _ _ _ -> True; _ -> False)
                          ((\(x,_,_)->x) (ml stream 2)))]
      where m ((TextEvent t):xs) i = (XText t,xs,i)
            m ((EmptyEvent n atts):xs) i = (XElem n atts i noParentError [],xs,i+1)
            m ((StartEvent n atts):xs) i
                = let (el,xs',i') = ml xs $! (i+1)
                  in (XElem n atts i noParentError el,xs',i')
            m ((PIEvent n s):xs) i = (XPI n s,xs,i)
            m ((CommentEvent s):xs) i = (XComment s,xs,i)
            m ((GERefEvent n):xs) i = (XGERef n,xs,i)
            m ((ErrorEvent s):xs) i = (XError s,xs,i)
            m (_:xs) i = (XError "unrecognized XML event",xs,i)
            m [] i = (XError "unbalanced tags",[],i)
            ml [] i = ([],[],i)
            ml ((EndEvent n):xs) i = ([],xs,i)
            ml xs i = let (e,xs',i') = m xs i
                          (el,xs'',i'') = ml xs' i'
                      in (e:el,xs'',i'')


-- Lazily materialize the SAX stream into a DOM tree setting parent references.
-- It has space leaks for large documents.
-- Used only if the query has backward steps that cannot be eliminated.
materializeWithParent :: Stream -> XTree
materializeWithParent stream = root
    where root = XElem "_document" [] 1 (XError "Trying to access the root parent")
                       [head (filter (\x -> case x of XElem _ _ _ _ _ -> True; _ -> False)
                                     ((\(x,_,_)->x) (ml stream 2 root)))]
          m ((TextEvent t):xs) i _ = (XText t,xs,i)
          m ((EmptyEvent n atts):xs) i p = (XElem n atts i p [],xs,i+1)
          m ((StartEvent n atts):xs) i p
              = let (el,xs',i') = (ml xs $! (i+1)) node
                    node = XElem n atts i p el
                in (node,xs',i')
          m ((PIEvent n s):xs) i _ = (XPI n s,xs,i)
          m ((CommentEvent s):xs) i _ = (XComment s,xs,i)
          m ((GERefEvent n):xs) i _ = (XGERef n,xs,i)
          m ((ErrorEvent s):xs) i _ = (XError s,xs,i)
          m (_:xs) i _ = (XError "unrecognized XML event",xs,i)
          m [] i _ = (XError "unbalanced tags",[],i)
          ml [] i _ = ([],[],i)
          ml ((EndEvent n):xs) i _ = ([],xs,i)
          ml xs i p = let (e,xs',i') = m xs i p
                          (el,xs'',i'') = ml xs' i' p
                      in (e:el,xs'',i'')


materialize :: Bool -> Stream -> XTree
materialize True = materializeWithParent
materialize False = materializeWithoutParent
