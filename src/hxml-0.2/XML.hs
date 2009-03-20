----------------------------------------------------------------------------
--
-- Module	: HXML.XML
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- CVS  	: $Id: XML.hs,v 1.9 2002/10/12 01:58:59 joe Exp $
--
----------------------------------------------------------------------------
--
-- 16 Jan 2000
-- Basic XML data types
--

module XML
    ( XMLNode(..)
    , Name, XML, AttList
    , stringValue, nodeName
    , xAttlist, xAttval
    , attributes, attval
    , xELNode, xTXNode, xPINode
    ) where

import Tree

-- | Element tagname or attribute name
type Name 	= String		-- %%% XMLNS makes this more complex
type GI 	= Name			-- generic identifier, element type name
-- | Attribute list
type AttList	= [(Name,String)]
type XML	= Tree XMLNode

data XMLNode =
      RTNode				-- root node
    | ELNode	GI AttList		-- element node: GI, attributes
    | TXNode	String			-- text node
    | PINode	Name String		-- processing instruction (target,value)
    | CXNode	String			-- comment node
    | ENNode	Name			-- general entity reference

    -- XPath also defines:
    --  ATNodeP	Name String		-- attribute node
    --  NSNodeP	Name {- prefix-} String {-URI-}	-- namespace node
    deriving Show

stringValue :: XML -> String		-- [XPATH, 5]
stringValue nd@(Tree d _) = case d of
    RTNode	-> concat [sv | TXNode sv <- preorderTree nd] 	-- [XPATH 5.1]
    ELNode _ _	-> concat [sv | TXNode sv <- preorderTree nd] 	-- [XPATH 5.2]
    TXNode s	-> s			-- [XPATH 5.7]
    PINode _ v	-> v			-- [XPATH 5.5]
    CXNode s	-> s			-- [XPATH 5.6]
    ENNode  _    -> ""			-- [not defined in XPATH]
    -- ATNodeP _ v	-> v		-- [XPATH 5.3]
    -- NSNodeP _ uri -> uri		-- [XPATH 5.4]

-- %%% need to fix this to account for [XMLNS]
nodeName :: XMLNode -> Maybe Name	-- [XPATH, 5; "expanded-name"]
nodeName nd = case nd of
    ELNode gi _	-> Just gi		-- %%% Check [XMLNS]
    PINode tgt _-> Just tgt		-- [XPATH 5.5], pi target, null URI
    ENNode name	-> Just name		-- [not defined in XPATH]
    --ATNodeP n _-> Just n		-- %%% Check [XMLNS]
    --NSNodeP p _-> Just p		-- [XPATH 5.4], ns prefix, null URI
    RTNode	-> Nothing		-- [XPATH 5.1]
    TXNode _	-> Nothing		-- [XPATH 5.7]
    CXNode _	-> Nothing		-- [XPATH 5.6]

--
-- Accessors:
--
xAttlist :: XMLNode -> AttList
xAttlist (ELNode _ attlist) 	= attlist
xAttlist _			= []

xAttval :: Name -> XMLNode -> Maybe String
xAttval name = lookup name . xAttlist

xELNode :: (Name -> AttList -> a)	-> XMLNode -> Maybe a
xTXNode :: (String -> a)		-> XMLNode -> Maybe a
xPINode :: (String -> String -> a)	-> XMLNode -> Maybe a

xELNode f (ELNode gi atts) 		= Just (f gi atts)
xELNode _ _		   		= Nothing
xTXNode f (TXNode txt) 			= Just (f txt)
xTXNode _ _				= Nothing
xPINode f (PINode tgt val) 		= Just (f tgt val)
xPINode _  _				= Nothing

--
-- Tree 
--

attributes :: XML -> AttList
attributes = xAttlist . treeRoot

attval :: Name -> XML -> Maybe String
attval name = xAttval name . treeRoot

-- EOF --
