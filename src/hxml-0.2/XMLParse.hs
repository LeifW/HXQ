{-# OPTIONS -fno-warn-missing-signatures #-}
----------------------------------------------------------------------------
--
-- Module	: XMLParse
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- CVS  	: $Id: XMLParse.hs,v 1.9 2002/10/12 01:58:59 joe Exp $
--
----------------------------------------------------------------------------
--
-- started 23 Jan 2000
--
-- TODO: expand parameter entity references in DTD and parameter literals
-- TODO: implement marked sections in DTD
--

module XMLParse
    ( XMLEvent(..)
    , parseInstance, parseDTD, parseDocument
    ) where

import XMLScanner
import LLParsing
import XML
import DTD
import Misc
import List(unfoldr)

parseInstance :: String -> [XMLEvent]

newtype UNPARSED = UNPARSED String	-- Unparsed literal
	deriving Show

replaceGERefs (UNPARSED s) = expandReferences DTD.predefinedEntities s
replacePERefs (UNPARSED s) = s		-- %%% FIX
attributeValueLiteral 	=   replaceGERefs <$> pLiteral
parameterLiteral 	=   replacePERefs <$> pLiteral
systemLiteral 		=   unparsed <$> pLiteral
			    where unparsed (UNPARSED s) = s

-- External interface:

data XMLEvent =
      StartEvent Name [(Name,String)]	-- start-tag (gi, attspecs)
    | EmptyEvent Name [(Name,String)]	-- empty element tag (gi,attspecs)
    | EndEvent   Name			-- end-tag (gi)
    | TextEvent	 String			-- character data (text)
    | PIEvent	 Name String		-- processing instruction (tgt value)
    | GERefEvent Name			-- general entity reference (ename)
    | CommentEvent String		-- comment
    | ErrorEvent String			-- error report
    	deriving (Read,Show)

-- Too strict: parseInstance = fmap fst . pRun (pList instanceItem) . pcdataMode
parseInstance = unfoldr (pRun instanceItem) . pcdataMode
parseDTD = foldl (\a b->b a) emptyDTD . unfoldr (pRun dtdItem) . markupMode

parseDocument text = 
    case pRun prolog (pcdataMode text) of
    	Just (_, rest)	-> unfoldr (pRun instanceItem) rest
	Nothing		-> [ErrorEvent "Error parsing prolog"]	-- can't happen.

-- Interface to scanner:

pDelim d 	= pTest (==d)
pKeyword kw	= pTest  (\d->case d of NAME n    -> n == kw; _ -> False)
rniName  kw	= pTest  (\d->case d of RNINAME n -> n == kw; _ -> False)
pName		= pCheck (\d->case d of NAME n    -> Just n ; _ -> Nothing)
pGEREF 		= pCheck (\d->case d of GEREF n   -> Just n ; _ -> Nothing)
pPEREF 		= pCheck (\d->case d of PEREF n   -> Just n ; _ -> Nothing)
pLiteral	= pCheck literal where
		  literal (LITERAL s)	= Just (UNPARSED s)
		  literal _		= Nothing
pCDATA		= pCheck cdata where
		  cdata (CDATA txt)	= Just txt
		  cdata (WS ws)		= Just ws
		  cdata _		= Nothing

-- Main grammar:

-- dtdItem :: Parser Delimiter (DTD -> DTD)
dtdItem =
	dtdDeclaration
    <|> const id <$> processingInstruction	-- %%% REPORT THESE
    <|> const id <$> sgmlCommentDeclaration
    <|> const id <$> pPEREF			-- %%% FIX

dtdDeclaration =
    pDelim MDO *> (
	    pKeyword "ELEMENT"  *> elementDeclaration
	<|> pKeyword "ATTLIST"  *> attlistDeclaration
	<|> pKeyword "ENTITY"   *> entityDeclaration
	<|> pKeyword "NOTATION" *> notationDeclaration
    ) <* pDelim MDC

prolog =
    pair <$> (ws *> pMaybe xmlDeclaration) <*> (ws *> pMaybe doctypeDeclaration)

ws = () <$ pList (pTest (\d -> case d of WS _ -> True ; _ -> False))

xmlDeclaration = processingInstruction

doctypeDeclaration = 
	pDelim MDO *> pKeyword "DOCTYPE" *> doctype <* pDelim MDC
doctype =
	() <$ pName <* externalIdentifier {-pMaybe internalSubset-}

--
-- Common constructs:
--

elementNames =
	wrap <$> pName <|> nameGroup
nameGroup =
    (:) <$ pDelim GRPO <*> pName <*>
	   (    (:) <$ pDelim SEQ <*> pName <*> pList (pDelim SEQ *> pName)
	    <|> (:) <$ pDelim OR  <*> pName <*> pList (pDelim OR  *> pName)
	    <|> (:) <$ pDelim AND <*> pName <*> pList (pDelim AND *> pName)
	    <|> pSucceed []
	   ) <* pDelim GRPC

externalIdentifier =
	pair Nothing
		<$  pKeyword "SYSTEM" <*> pMaybe systemLiteral
    <|> pair	<$  pKeyword "PUBLIC"
		<*> (Just <$> systemLiteral) <*> pMaybe systemLiteral
    <|> pSucceed (Nothing,Nothing)	-- implicit identifier

-- This works for XML:
xmlCommentDeclaration =
    pDelim MDOCOM *> pcdata <* pDelim COM <* pDelim MDC
    where pcdata = pFoldr (++) [] pCDATA

-- This works for SGML:
sgmlCommentDeclaration =
    (++) <$ pDelim MDOCOM <*> (pcdata <* pDelim COM) <*> comments <* pDelim MDC
    where pcdata   = pFoldr (++) [] pCDATA
	  comments = pFoldr (++) [] (pDelim COM *> pcdata <* pDelim COM)

processingInstruction =
    makePI . concat <$ pDelim PIO <*> pList pCDATA <* pDelim PIC where 
	makePI string =
	    let (pitgt, rest)	= span isNMCHAR string
		pival 		= dropWhile isSEPCHAR rest
	    in (pitgt,pival)

--
-- Instance items:
--

instanceItem =
    	pTag
    <|> TextEvent		<$> pCDATA	 -- also: WSEvent
    <|> GERefEvent		<$> pGEREF
    <|> uncurry PIEvent		<$> processingInstruction
    <|> CommentEvent 		<$> xmlCommentDeclaration

pTag =
	startEvent <$ pDelim STAGO <*> pName <*> attributes <*> tagc
    <|> EndEvent   <$ pDelim ETAGO <*> pName                <*  pDelim TAGC
    where
	attributes = pList (pName <* pDelim VI <^> attributeValue)
	tagc = StartEvent <$ pDelim TAGC 
	   <|> EmptyEvent <$ pDelim EETAGC
    	startEvent name atts closing = closing name atts

attributeValue =	-- XML: attributeValueLiteral only
	attributeValueLiteral <|> pName

--
-- <!ELEMENT ...> declarations
--
elementDeclaration =
    declareElements
    <$> elementNames <*> omissibility <*> contentDefinition <*> exceptions
    where
	omissibility =
	    (pair <$> dashoro <*> dashoro) <?> (False,False)
	dashoro =
	    (True <$ pKeyword "O" <|> False <$ pDelim MINUS)
	exceptions =	-- NOTE ambiguity
	    pair <$> (pDelim MINUS *> nameGroup <?> [])
		 <*> (pDelim PLUS  *> nameGroup <?> [])

contentDefinition = -- in SGML: "declared content-or-content-model-w/oexc"
	DC_EMPTY	<$  pKeyword "EMPTY"
    <|> DC_ANY		<$  pKeyword "ANY"
    <|> DC_MODELGRP	<$> contentModel

contentModel =
    (     Prim . ELEMENT <$> pName
      <|> Prim PCDATA    <$  rniName "#PCDATA"
      <|> pDelim GRPO *> contentModel <**>
	    (     mk Seq <$> pSome (pDelim SEQ *> contentModel)
	      <|> mk And <$> pSome (pDelim AND *> contentModel)
	      <|> mk Or  <$> pSome (pDelim OR  *> contentModel)
	      <|> pSucceed id ) <* pDelim GRPC
    ) <**> occurrenceIndicator
	where mk f l a = f (a:l)

occurrenceIndicator =
	Plus <$ pDelim PLUS
    <|> Rep  <$ pDelim REP
    <|> Opt  <$ pDelim OPT
    <|> pSucceed id

--
-- <!ATTLIST ...> declarations:
--
-- Also need: <!ATTLIST #NOTATION ... > , <!ATTLIST #ANY ...>
--
attlistDeclaration =
    declareAttlist <$> elementNames <*> pList attributeDefinition

attributeDefinition =
    ATTDEF <$> pName <*> declaredValue <*> defaultValue
declaredValue =
	ATcdata		<$  pKeyword "CDATA"
    <|> ATid    	<$  pKeyword "ID"
    <|> ATidref 	<$  pKeyword "IDREF"
    <|> ATidrefs	<$  pKeyword "IDREFS"
    <|> ATentity	<$  pKeyword "ENTITY"
    <|> ATentities	<$  pKeyword "ENTITIES"
    <|> ATnmtoken	<$  pKeyword "NMTOKEN"
    <|> ATnmtokens	<$  pKeyword "NMTOKENS"
    <|> ATnotation	<$  pKeyword "NOTATION" <*> nameGroup
    <|> ATenumerated	<$> nameGroup
    -- SGML only: (NB: currently ignore distinction between these)
    <|> ATnmtoken	<$  pKeyword "NAME"
    <|> ATnmtoken	<$  pKeyword "NUMBER"
    <|> ATnmtoken	<$  pKeyword "NUTOKEN"
    <|> ATnmtokens 	<$  pKeyword "NAMES"
    <|> ATnmtokens 	<$  pKeyword "NUMBERS"
    <|> ATnmtokens 	<$  pKeyword "NUTOKENS"
defaultValue =		-- SGML: also #CURRENT, #CONREF
	ADVimplied	<$  rniName "#IMPLIED"
    <|> ADVrequired	<$  rniName "#REQUIRED"
    <|> ADVfixed	<$  rniName "#FIXED" <*> attributeValue
    <|> ADVdefault	<$> attributeValue
    -- SGML only:
    <|> ADVcurrent	<$  rniName "#CURRENT"
    <|> ADVconref	<$  rniName "#CONREF"

-- Entity declarations:
-- %%% In SGML grammar there are a few more restrictions

entityDeclaration =
	declareParameterEntity <$  pDelim PERO <*> pName <*> entityText
    <|> declareGeneralEntity   <$>                 pName <*> entityText

-- @@@ Discards entity type, DCN, and data attributes
entityText =
	EN_INTERNAL <$> parameterLiteral
    <|> EN_EXTERNAL <$> externalIdentifier <* entityType where
	entityType =
		ETsubdoc <$  pKeyword "SUBDOC"
	    <|> csndata <* pName <* dataAttributes
	dataAttributes =
		pDelim DSO
	     *> pList (pair <$> pName <* pDelim VI <*> attributeValue)
	    <* pDelim DSC
	csndata = 
		ETcdata 	<$  pKeyword "CDATA" 
	    <|> ETsdata 	<$  pKeyword "SDATA"
	    <|> ETndata 	<$  pKeyword "NDATA"

notationDeclaration =
	declareNotation <$> pName <*> externalIdentifier

-- SGML: also need SHORTREF, USEMAP(1) in DTDs;
-- LINKTYPE in prolog; USEMAP(2), USELINK in instance.

-- EOF --
