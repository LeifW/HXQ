----------------------------------------------------------------------------
--
-- Module	: XMLScanner
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- CVS  	: $Id: XMLScanner.hs,v 1.9 2002/10/12 01:58:59 joe Exp $
--
----------------------------------------------------------------------------
--
-- 9 Jan 2000
--

-- doesn't check as many errors as it ought to...

module XMLScanner
    ( Delimiter(..)
    , pcdataMode, markupMode
    , isNMCHAR, isSEPCHAR
    , expandReferences
    ) where

import XML		-- for "Name"
import Char

import qualified DTD

isSEPCHAR, isNMCHAR, isNMSTART :: Char -> Bool

isSEPCHAR 	= isSpace
isNMCHAR c	= isAlphaNum c || c `elem` ".-_:"	-- 2.3 prodn 4
--isNMSTART c	= isAlpha c    || c `elem` "_:"		-- 2.3 prodn 5
isNMSTART c	= isAlphaNum c || c `elem` "_:"	-- for NUTOKEN, NMTOKEN attvals

-- Utility:
--
-- doSpan pred k f g s = k (f x) (g y) where (x,y) = span pred s
--
doSpan :: (a -> Bool) -> (b -> c -> d) -> ([a] -> b) -> ([a] -> c) -> [a] -> d
doSpan pred k f g = sp f where
    sp f' [] 		= k (f' []) (g [])
    sp f' s@(c:cs)
    	| pred c 	= sp (f' . (c:)) cs
	| otherwise	= k (f' []) (g s)

drop1 :: [a] -> [a]	 -- drop1 == drop 1; safe version of tail
drop1 [] 	= []
drop1 (_:xs)	= xs

data Delimiter =
    -- character data mode:
      WS String		-- whitespace
    | CDATA String	-- character data
    | GEREF Name 	-- general entity reference
    | STAGO		-- start tag open, "<"
    | ETAGO		-- end tag open, "</"

    -- Markup mode and character data mode:
    | MDO	-- markup declaration open, "<!"
	| MDOCOM	-- MDO + COM delimiter-in-context, "<!--"
	| MDODSO	-- MDO + DSO delimiter-in-context, "<!["
    | PIO	-- processing instruction open, "<?"
    | PIC	-- processing instruction close, "?>" (">" in SGML)

    -- Errors:
    | LEXERR String	-- lexical error
    | REST String	-- rest of input

    -- Markup mode:
    | NAME Name 	-- name
    | RNINAME Name	-- name prefixed with RNI (#)
    | PEREF Name	-- parameter entity reference
    | LITERAL String	-- attribute value literal or parameter literal

    | TAGC	-- tag close, ">"
    | VI	-- value indicator, "="
    | EETAGC	-- empty element tag close (XML), "/>"

    | MDC	-- markup declaration close, ">"
    | DSO	-- declaration subset open,  "["
    | DSC	-- declaration subset close, "]"
    | MSC	-- DSC+MDC, delimiter-in-context, "]]>"
    | COM	-- comment, "--"
    | GRPO	-- group open, "("
    | GRPC	-- group close, ")"
    | AND	-- and connector, "&"
    | OR	-- or connector, "|"
    | SEQ	-- seq connector, ","
    | OPT	-- opt occurrence indicator, "?"
    | REP	-- rep occurrence indicator, "*"
    | PLUS	-- plus occurrence indicator, inclusion, "+"
    | MINUS	-- exclusion, omission flag, "-"
    | PERO	-- parameter entity reference open, "%"
    -- ALSO:
    -- Shortref	-- short reference string (SGML only)
    -- NET		-- null end-tag (SGML only)
    -- RNI	-- reserved name indicator, "#"
    -- LIT	-- literal, """
    -- LITA	-- alternative literal, "'"
    -- CRO	-- character reference open, "&#"
    -- HCRO	-- hex character reference open, "&#X" (new in XML)
    -- ERO	-- entity reference open, "&"
    -- REFC	-- reference close, ";"
  deriving (Eq, Show)

pcdataMode, tagMode, markupMode :: String -> [Delimiter]

-- [STAGO, ETAGO, NET, CRO, ERO, MDO, MDOCOM, MDODSO, PIO, MSC]
pcdataMode [] = []
pcdataMode ('<':s) = case s of
    '!':'-':'-':r	-> MDOCOM : comMode pcdataMode r
    '!':'[':r		-> {- MDODSO : -} msMode r
    '!':r 		-> MDO : markupMode r
    '/':r		-> ETAGO : tagMode r
    '?':r		-> PIO : piMode pcdataMode r
    ']':']':'>':r	-> MSC : pcdataMode r
    r			-> STAGO : tagMode r
pcdataMode ('&':'#':s) = doSpan (';'/=) (:) mkCREF (pcdataMode . drop1) s
	where mkCREF = CDATA . return . chr . stringToInt 10
pcdataMode ('&':s) =
	case span isNMCHAR s of
	    (ename,';':r)	-> GEREF ename : pcdataMode r
	    (junk,r)		-> LEXERR ("Bad entity reference " ++ junk)
				    : pcdataMode r
pcdataMode ('>':r) = LEXERR "Warning: %%% SKIPPING UNESCAPED '>'":pcdataMode r
pcdataMode (c:s)
    | isSEPCHAR c = doSpan isSEPCHAR  (:) (WS . (c:)) pcdataMode s
    | otherwise	  = doSpan isDATACHAR (:) (CDATA . (c:)) pcdataMode s
		    where  isDATACHAR ch =
			      case ch of '<' -> False; '&' ->False; _ -> True

tagMode []		= []
tagMode ('/':'>':r) 	= EETAGC : pcdataMode r
tagMode ('>':r)		= TAGC   : pcdataMode r
tagMode ('=':r)  	= VI     : tagMode r
tagMode ('"':r)  	= doSpan ('"'/=)  (:) LITERAL (tagMode . drop1) r
tagMode ('\'':r) 	= doSpan ('\''/=) (:) LITERAL (tagMode . drop1) r
tagMode ('<':'/':r) 	= ETAGO  : tagMode r		-- not allowed in XML
tagMode ('<':r)     	= STAGO  : tagMode r		-- not allowed in XML
tagMode cs@(c:s)
    | isSEPCHAR c	= tagMode (dropWhile isSEPCHAR s)
    | isNMSTART c	= doSpan isNMCHAR (:) NAME tagMode cs
    | otherwise		= LEXERR [c] : tagMode s


-- [ERO, CRO, HCRO]
expandReferences :: DTD.EntityMap -> String -> String
expandReferences entities = expand where
    expand s = case s of
	[]		-> []
	'&':'#':'X':r	-> doCharRef 16 expand r
	'&':'#':r	-> doCharRef 10 expand r
	'&':r		-> doEntityRef entities expand r
	x:r		-> x : expand r

doCharRef :: Int -> (String -> String) -> [Char] -> [Char]
doCharRef base k = doSpan (';'/=) (:) (chr . stringToInt base) (k . drop1)
stringToInt :: Int -> String -> Int
stringToInt base = foldl digit 0 . map (\x -> if isDigit x then digitToInt x else 0)
    where digit num next = base*num + next

-- @@@ This is not quite right: should rescan the replacement text.
doEntityRef :: DTD.EntityMap -> (String -> String) -> String -> String
doEntityRef entities k r = doSpan (';'/=) (++) replacement (k . drop1) r where
    replacement ename = case DTD.expandInternalEntity entities ename of
	    Just s 	-> s
-- changed by Leonidas Fegaras 5/27/08
	    _		-> "" -- error ("entity " ++ ename ++ " not defined")

-- [...]
markupMode []	= []
markupMode ('%':s)	= case span isNMCHAR s of
	([], ' ':r)	-> PERO : markupMode r	-- %%% Not Quite Right
	(ename,';':r)	-> PEREF ename : markupMode r
	(ename,r)	-> LEXERR ("Bad parameter entity reference %" ++ ename)
				: markupMode r
markupMode ('-':'-':r) 	= eatComment r
markupMode ('>':r)  	= MDC : markupMode r	-- %%% or pcdataMode?
markupMode ('"':r)  	= doSpan ('"'/=)  (:) LITERAL (markupMode . drop1) r
markupMode ('\'':r) 	= doSpan ('\''/=) (:) LITERAL (markupMode . drop1) r
markupMode ('#':r)	= doSpan isNMCHAR (:) (RNINAME . ('#':)) markupMode r
markupMode ('<':'!':'-':'-':r)
			= MDOCOM : comMode markupMode r
markupMode ('<':'!':r)	= MDO : markupMode r
markupMode ('<':'?':r)	= PIO : piMode markupMode r
markupMode s@('<':_)	= pcdataMode s	-- %%% Not strictly correct, but
					-- %%% needed to parse the prolog.
markupMode cs@(c:s)
    | isSEPCHAR c	= markupMode (dropWhile isSEPCHAR s)
    | isNMSTART c	= doSpan isNMCHAR (:) NAME markupMode cs
    | otherwise = (case c of
	'&'	-> AND
	'|'	-> OR
	','	-> SEQ
	'?'	-> OPT
	'*'	-> REP
	'+'	-> PLUS
	'-'	-> MINUS
	'['	-> DSO
	']'	-> DSC
	'('	-> GRPO
	')'	-> GRPC
	_	-> LEXERR [c]) : markupMode s

--
-- Internal recognition modes:
--

msMode, cdataMode, eatComment :: String -> [Delimiter]

piMode, comMode, cdMode :: (String -> [Delimiter]) -> String -> [Delimiter]

--
-- msMode: marked section in instance. 
-- @@@ Only supports XML instance syntax (<![CDATA[ ... ]]>);
-- In SGML (and XML DTDs), parameter entity references and whitespace
-- are also allowed, in addition to INCLUDE and IGNORE keywords.
-- 'cdataMode' checks for nested occurrences of <![. this is not
-- an error according to the XML or SGML specs, but it ought to be.
-- 

msMode ('C':'D':'A':'T':'A':'[':rest) = cdataMode rest
msMode s = 
    let (ms, rest) = span ('['/=) s
    in LEXERR ("Illegal marked section ["++ms) : pcdataMode (drop1 rest)

cdataMode (']':']':'>':rest) = pcdataMode rest
cdataMode ('<':'!':'[':rest) = error "Nested <![ in marked section"
cdataMode [] = []
cdataMode (c:cs) = doSpan spn (:) (CDATA . (c:)) cdataMode cs where
	spn '\n' = False
	spn ']'  = False
	spn '<'  = False
	spn _    = True

--
-- comMode (inside comments):  [COM]
--
comMode prevMode cs = case cs of
    []		-> []
    '-':'-':r	-> COM : cdMode prevMode r
    (c:s)	-> doSpan brk (:) (CDATA . (c:)) (comMode prevMode) s where
    			brk '-' = False
			brk '\n' = False -- split long comments at line breaks
			brk _ = True

-- cdMode (inside comment declarations): [COM,MDC, ignore whitespace]
cdMode prevMode cs = case cs of
    []		-> []
    '>':r	-> MDC : prevMode r
    '-':'-':r	-> COM : comMode prevMode r
    c:s 	-> if isSEPCHAR c
    	           then cdMode prevMode (dropWhile isSEPCHAR s)
		   else LEXERR [c] : cdMode prevMode s

eatComment cs = case cs of
    []		-> []
    '-':'-':r	-> markupMode r
    (_:r)	-> eatComment r

piMode prevMode cs = case cs of
    []		-> []
    '?':'>':r	-> PIC : prevMode r
    (c:s)	-> doSpan ('?'/=) (:) (CDATA . (c:)) (piMode prevMode) s

-- EOF --
