----------------------------------------------------------------------------
--
-- Module	: HXML.ETree
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- CVS  	: $Id: ETree.hs,v 1.3 2002/10/12 01:58:57 joe Exp $
--
----------------------------------------------------------------------------
--
-- 11 Mar 2002
--
-- Simplified XML representation with only the essential Infoset properties.
--

module ETree(ETree, xmlToETree, etreeToXML) where

import XML
import Tree

data ETree
    = Element Name AttList [ETree]
    | Text String
    deriving Show

xmlToETree :: XML -> ETree
xmlToETree = maybe fallback id . foldTree etree mcons [] where
    etree (TXNode txt) _	= Just $ Text txt
    etree (ELNode gi atts) c	= Just $ Element gi atts c
    etree RTNode (c:_) 		= Just c
    etree _ _			= Nothing
    mcons 			= maybe id (:)
    fallback			= Text "Error: ill-formed XML input"

etreeToXML :: ETree -> XML
etreeToXML = anaTree psi where
    psi (Text txt)			= (TXNode txt,[])
    psi (Element gi atts content)	= (ELNode gi atts,content)

-- EOF --
