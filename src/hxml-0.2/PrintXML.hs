----------------------------------------------------------------------------
--
-- Module	: HXML.PrintXML
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- CVS  	: $Id: PrintXML.hs,v 1.5 2002/10/12 01:58:58 joe Exp $
--
----------------------------------------------------------------------------

module PrintXML 
    ( printXML, showXML
    , printEvent, showEvent, showEvents, printEvents
    ) where

import XML
import Tree
import TreeBuild
import XMLParse (XMLEvent(..))

printXML :: XML -> IO ()
printXML = printEvents . serializeTree

showXML :: XML -> String
showXML t = pp t [] where
    pp (Tree nd children) k = case nd of
	RTNode 			-> ppl children k
	TXNode txt 		-> textEscape txt k
	PINode tgt [] 		-> "<?" ++ tgt ++ "?>" ++ k
	PINode tgt val 		-> "<?" ++ tgt ++ " " ++ val ++ "?>" ++ k
	CXNode txt 		-> "<!--" ++ txt ++ "-->" ++ k
	ENNode ename 		-> "&" ++ ename ++ ";" ++ k
	ELNode gi attlist	->
	    let atts = showAttlist attlist
	    in case children of
		[] -> "<" ++ gi ++ atts ++ "/>" ++ k
		_  -> "<" ++ gi ++ atts ++ ">"
		      ++ ppl children ("</" ++ gi ++ "\n>" ++ k)
    ppl [] k = k
    ppl (x:xs) k = pp x (ppl xs k)

showEvent	:: XMLEvent -> String
printEvent	:: XMLEvent -> IO ()
showEvents	:: [XMLEvent] -> String
printEvents	:: [XMLEvent] -> IO ()

showEvents 	= concatMap showEvent
printEvent	= putStr . showEvent
printEvents	= mapM_ printEvent

showEvent (StartEvent gi atts) 	= "<" ++ gi ++ showAttlist atts ++ ">"
showEvent (EmptyEvent gi atts)	= "<" ++ gi ++ showAttlist atts ++ "/>"
showEvent (EndEvent   gi)	= "</" ++ gi ++ "\n>"
showEvent (TextEvent  txt)	= textEscape txt []
showEvent (PIEvent    tgt [])	= "<?" ++ tgt ++ "?>"
showEvent (PIEvent    tgt val)	= "<?" ++ tgt ++ " " ++ val ++ "?>"
showEvent (GERefEvent name)	= "&" ++ name ++ ";"
showEvent (CommentEvent txt)	= "<--" ++ txt ++ "-->"
showEvent (ErrorEvent txt)	= error txt

showAttlist :: [(Name,String)] -> String
showAttlist attlist = concat [' ':patt nm val | (nm,val) <- attlist]
    where
	vi = "="
	patt nm val = nm ++ vi ++ "\"" ++ attvalEscape val "\""

textEscape, attvalEscape :: String -> ShowS

textEscape [] k = k
textEscape (c:cs) k =
    case c of
	'<'	-> "&lt;" ++ textEscape cs k
	'>'	-> "&gt;" ++ textEscape cs k
	'&'	-> "&amp;" ++ textEscape cs k
	_	-> c : textEscape cs k

attvalEscape [] k = k
attvalEscape (c:cs) k =
    case c of
	'<'	-> "&lt;" ++ attvalEscape cs k
	'>'	-> "&gt;" ++ attvalEscape cs k
	'&'	-> "&amp;" ++ attvalEscape cs k
	'\''	-> "&apos;" ++ attvalEscape cs k
	'\"'	-> "&quot;" ++ attvalEscape cs k
	_	-> c : attvalEscape cs k

-- EOF --
