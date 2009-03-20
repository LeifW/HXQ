----------------------------------------------------------------------------
--
-- Module	: HXML.Tree
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- CVS  	: $Id: Tree.hs,v 1.9 2002/10/12 01:58:58 joe Exp $
--
----------------------------------------------------------------------------
--
-- 7 Jan 2000
--

module Tree where

data Tree a = Tree a [Tree a]
	deriving Show

--
-- Projections:
--
treeRoot    	:: Tree a -> a
treeChildren	:: Tree a -> [Tree a]
treeRoot  	(Tree a _) = a
treeChildren	(Tree _ c) = c

leafNode :: a -> Tree a
leafNode x = Tree x []

-- preorderTree (Tree a c) = a : concatMap preorderTree c
-- preorderTree = cataTree(\(x,bs) -> x : concat bs)
preorderTree :: Tree a -> [a]
preorderTree t = traverse t [] where
    traverse (Tree a c) k 	= a : travlist c k
    travlist (c:cs) k		= traverse c (travlist cs k)
    travlist [] k 		= k

-- The usual polytypic routines:

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Tree a c) = Tree (f a) (map (mapTree f) c)

instance Functor Tree where fmap = mapTree

-- type TreeF a b = (a, [b])
cataTree :: ((a, [b]) -> b) -> Tree a -> b -- (TreeF a b -> b) -> Tree a -> b
anaTree  :: (b -> (a, [b])) -> b -> Tree a -- (b -> TreeF a b) -> b -> Tree a
cataTree f (Tree a c) = f (a,map (cataTree f) c)
anaTree g b = let (a,bs) = g b in Tree a (map (anaTree g) bs)

-- A friendlier variant of cataTree:
--
foldTree :: (a -> b -> c) -> (c -> b -> b) -> b -> Tree a -> c
foldTree tree cons nil (Tree a c)
	= tree a (foldr cons nil (map (foldTree tree cons nil) c))

-- Downwards accumulation:
--
scanTree :: (a -> b -> a) -> a -> Tree b -> Tree a
scanTree op a (Tree b children)
	= let a' = a `op` b in Tree a' (map (scanTree op a') children)

-- A variant:
--
accumTree :: (a -> b -> (c, a)) -> a -> Tree b -> Tree c
accumTree op a (Tree b children)
	= let (c,a') = a `op` b in Tree c (map (accumTree op a') children)

-- EOF --
