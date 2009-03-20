----------------------------------------------------------------------------
--
-- Module	: HXML
-- Copyright	: (C) 2001-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- Created	: 1 Nov 2001
-- CVS  	: $Id: HXML.hs,v 1.5 2002/10/12 01:58:57 joe Exp $
--
----------------------------------------------------------------------------
--
-- | Package module for HXML -- just reexports all the public modules.
--

module HXML 
    ( module XML
    , module Tree
    , module PrintXML
    , module ETree

    , parseXML
    ) where

import XML
import XMLParse
import Tree
import TreeBuild
import PrintXML
import ETree

parseXML :: String -> Tree XMLNode
parseXML = buildTree . parseDocument

-- EOF --
