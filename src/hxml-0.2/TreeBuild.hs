----------------------------------------------------------------------------
--
-- Module	: HXML.TreeBuild
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- CVS  	: $Id: TreeBuild.hs,v 1.7 2002/10/12 01:58:58 joe Exp $
--
----------------------------------------------------------------------------
--
-- 30 Jan 2000
--

module TreeBuild (buildTree, constructTree, serializeTree) where

import XMLParse
import XML
import Tree

--
-- TODO: add basic error-checks: matching end-tags, ensure input exhausted
--
-- %%% There is apparently a space leak here, but I can't find it.
-- %%% Update 28 Feb 2000: There is a leak, but it's fixed
-- %%% by a well-known GC implementation technique.  Hugs 98 happens
-- %%% not to implement this technique, but STG Hugs (and most other
-- %%% Haskell systems) do implement it.
-- %%% Thanks to Simon Peyton-Jones, Malcolm Wallace, Colin Runcinman
-- %%% Mark Jones, and others for investigating this.

buildTree :: [XMLEvent] -> Tree XMLNode
buildTree = constructTree Tree (:) []

constructTree :: (XMLNode -> f -> t) -> (t -> f -> f) -> f -> [XMLEvent] -> t
constructTree tree cons nil events = let
	pair x y 		= (x,y)
	addNode nd children es	= addTree (tree nd children) es
	addLeaf nd es		= addTree (tree nd nil) es
	addTree t es		= let (s,es') = build es in pair (cons t s) es'
	build [] 		= pair nil []
	build (e:es) = case e of
	    StartEvent gi atts	-> let (c,es') = build es 
	    			   in addNode (ELNode gi atts) c es'
	    EndEvent _		-> pair nil es
	    EmptyEvent gi atts	-> addLeaf (ELNode gi atts) es
	    TextEvent s		-> addLeaf (TXNode s) es
	    PIEvent tgt val	-> addLeaf (PINode tgt val) es
	    CommentEvent txt	-> addLeaf (CXNode txt) es
	    GERefEvent name	-> addLeaf (ENNode name) es
	    ErrorEvent s	-> error s  -- %%% deal with this
	in tree RTNode (fst (build events))

serializeTree :: Tree XMLNode -> [XMLEvent]
serializeTree tree = sn tree [] where
    sn (Tree node content) k = case node of
	RTNode 		-> sl content k
	ELNode gi atts	-> StartEvent gi atts : sl content (EndEvent gi : k)
	TXNode txt	-> TextEvent txt : k
	PINode tgt val	-> PIEvent tgt val : k
	CXNode txt	-> CommentEvent txt : k
	ENNode name	-> GERefEvent name : k
    sl [] k 	= k
    sl (x:xs) k = sn x (sl xs k)

-- EOF --
