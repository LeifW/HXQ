----------------------------------------------------------------------------
--
-- Module	: HXML.Misc
-- Copyright	: (C) 2000-2002 Joe English.  Freely redistributable.
-- License	: "MIT-style"
--
-- Author	: Joe English <jenglish@flightlab.com>
-- Stability	: experimental
-- Portability	: portable
--
-- CVS  	: $Id: Misc.hs,v 1.8 2002/10/12 01:58:58 joe Exp $
--
----------------------------------------------------------------------------
--
-- 21 Jan 2000
-- Miscellaneous combinators that I find useful
--
 
module Misc where

errNYI :: String -> a
errNYI msg = error ("Not Yet Implemented: " ++ msg)

-- Kleisli composition:
o :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f `o` g = \x -> g x >>= f

-- Some useful anamorphisms:
maybeStar, maybePlus :: (a -> Maybe a) -> a -> [a]
maybeStar f a = a : maybe [] (maybeStar f) (f a)
maybePlus f a =     maybe [] (maybeStar f) (f a)

-- Used to be in Haskell Prelude:
done :: Monad m => m ()
done = return ()

-- H98: found in module Monad:
liftM2 :: (Monad m) => (b->c->d) -> m b -> m c -> m d
liftM2 op x y = x >>= \a ->  y >>= \b -> return (op a b)

-- H98: found in module Maybe:
maybeToList :: Maybe a -> [a]
maybeToList Nothing 	= []
maybeToList (Just a)	= [a]

-- ... other stuff
{- Removed by Leonidas Fegaras because it classes with the profiler
instance Monad ((->) s) where		-- Reader Monad
    return	= const
    f >>= g  	= \x -> g (f x) x
-}

lift :: (b->c->d) -> (a->b) -> (a->c) -> (a->d)
lift f g h x = f (g x) (h x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

wrap :: a -> [a]
wrap x = [x]

-- EOF --
