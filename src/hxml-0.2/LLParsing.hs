{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fglasgow-exts #-}
----------------------------------------------------------------------------
--
-- Module	: HXML.LLParsing
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable (wants rank-2 polymorphism but can live without it)
--
-- CVS  	: $Id: LLParsing.hs,v 1.6 2002/10/12 01:58:57 joe Exp $
--
----------------------------------------------------------------------------
--
-- 20 Jan 2000
-- Simple, non-backtracking, no-lookahead parser combinators.
-- Use with caution!
--

module LLParsing
    ( pTest , pCheck , pSym , pSucceed
    ,(<|>),(<*>),(<$>),(<^>),(<$),(<*),(*>),(<?>),(<**>)
    , pMaybe , pFoldr , pList , pSome , pChainr , pChainl, pTry
    , pRun
    ) where

infixl 3 <|>
infixl 4 <*>, <$>, <^>, <?>, <$, <*, *>, <**>

{- <H98> -}
-- Use this for Haskell 98:
newtype P p = P p
{- </H98> -}

{- <H98EXT> -}
{-
-- Use this if the system supports rank-2 polymorphism:
newtype Parser sym res = P (forall a .
       (res -> [sym] -> a)		-- ok continuation
    -> ([sym] -> a) 			-- failure continuation
    -> ([sym] -> a) 			-- error continuation
    -> [sym] 				-- input
    -> a)				-- result

pTest	:: (a -> Bool) -> Parser a a
pCheck	:: (a -> Maybe b) -> Parser a b
pSym	:: (Eq a) => a -> Parser a a
pSucceed:: b -> Parser a b
(<|>)	:: Parser a b -> Parser a b -> Parser a b	-- union
(<*>)	:: Parser a (b->c) -> Parser a b -> Parser a c	-- sequence
(<$>) 	:: (b->c) -> Parser a b -> Parser a c		-- application
(<$ )	:: c -> Parser a b -> Parser a c		-- application, dropr
(<^>)	:: Parser a b -> Parser a c -> Parser a (b,c)	-- sequence
(<* )	:: Parser a b -> Parser a c -> Parser a b	-- sequence, dropr
( *>)	:: Parser a b -> Parser a c -> Parser a c	-- sequence, dropl
(<?>)	:: Parser a b -> b -> Parser a b		-- optional
(<**>)	:: Parser s b -> Parser s (b->a) -> Parser s a	-- postfix application
pMaybe	:: Parser s a -> Parser s (Maybe a)
pFoldr	:: (a->b->b) -> b -> Parser s a -> Parser s b
pList	:: Parser a b -> Parser a [b]
pSome	:: Parser a b -> Parser a [b]
pChainr	:: Parser a (b -> b -> b) -> Parser a b -> Parser a b
pChainl	:: Parser a (b -> b -> b) -> Parser a b -> Parser a b
pRun 	:: Parser a b -> [a] -> Maybe (b,[a])
-}
{- </H98EXT> -}

pTest pred = P (ptest pred)
    where
    ptest _p _o f _e [] = f []
    ptest p ok f _e l@(c:cs)
	| p c		= ok c cs
	| otherwise	= f l

pSym a = pTest (a==)
pCheck cmf = P (pcheck cmf) where
    pcheck _mf _ok f _e [] = f []
    pcheck mf ok f _e cs@(c:s) = case (mf c) of
	Just x	-> ok x s
	Nothing	-> f cs

pTry (P pa) = P (\ok f _e i -> pa ok f (\ _i' -> f i) i)

pSucceed a 		= P (\ok _f _e  -> ok a)
(P pa) <|> (P pb) 	= P (\ok f e  	-> pa ok (pb ok f e) e)
(P pa) <*> (P pb) 	= P (\ok f e  	-> pa (\a -> pb (ok . a) e e) f e)
(P pa) <?>  a		= P (\ok _f  	-> pa ok (ok a))
(P pa) <^> (P pb)	= P (\ok f e	-> pa (\a->pb(\b->ok (a,b)) e e) f e)
f      <$> (P pb)	= P (\ok 	-> pb (ok . f))
f      <$  (P pb)	= P (\ok 	-> pb (ok . const f))
pa     <*     pb  	= curry fst <$> pa <*> pb
pa      *>    pb 	= curry snd <$> pa <*> pb
pa    <**>    pb 	= (\x f -> f x) <$> pa <*> pb

pMaybe p		= Just <$> p <|> pSucceed Nothing
pFoldr op e p 		= loop where loop = (op <$> p <*> loop) <?> e
pList 			= pFoldr (:) []
pSome p           	= (:) <$> p <*> pList p
pChainr op p 		= loop
			  where loop = p <**> ((flip <$> op <*> loop) <?> id)
pChainl op p 		= foldl ap <$> p <*> pList (flip <$> op <*> p)
			  where ap x f = f x

pRun (P p) = p just2 fail fail where
    just2 x y		= Just (x,y)
    fail 		= const Nothing

-- EOF --
