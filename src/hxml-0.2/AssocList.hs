----------------------------------------------------------------------------
--
-- Module	: AssocList.hs
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: provisional
-- Portability	: portable
--
-- CVS  	: $Id: AssocList.hs,v 1.5 2002/10/12 01:58:56 joe Exp $
--
----------------------------------------------------------------------------
--
-- Quick hack; need a stub FiniteMap implementation
--

module AssocList 
    ( FM, unsafeLookup, lookupM, lookupWithDefault, empty
    , insert , insertWith
    ) where

import Prelude -- hiding (null,map,foldr,foldl,foldr1,foldl1,filter)

type FM k a = [(k,a)]

lookupM :: (Eq k) => FM k a -> k -> Maybe a
lookupM = flip Prelude.lookup

lookupWithDefault :: (Eq key) => FM key elt -> elt -> key -> elt
lookupWithDefault m d = maybe d id . lookupM m

unsafeLookup :: (Eq a) => FM a b -> a -> b
unsafeLookup m = maybe (error "Error: Not found") id . lookupM m

insertWith :: (Eq k) => (a -> a -> a) -> k -> a -> FM k a -> FM k a
insertWith _ key elt [] = [(key,elt)]
insertWith c key elt ((k,e):l)
	| k == key	= (k,c e elt):l
	| otherwise	= (k,e):insertWith c key elt l

insert :: (Eq k) => k -> a -> FM k a -> FM k a
insert = insertWith (\_old new -> new)

{-
-- GHC 'data' library convention:
addToFM_C :: (elt -> elt -> elt) -> FM key elt -> key -> elt -> FM key elt
addToFM :: FM key elt -> key -> elt  -> FM key elt
lookupFM :: FM key elt -> key -> Maybe elt
lookupWithDefaultFM :: FM key elt -> elt -> key -> elt
-}

empty :: FM a b
empty = []

-- EOF --
