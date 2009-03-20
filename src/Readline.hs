{-# OPTIONS -cpp #-}

module Readline (readline,addHistory) where

#if __GLASGOW_HASKELL__ >= 609
import System.Console.Editline.Readline
#else
import System.Console.Readline
#endif

{-
import System.IO

readline prompt = do putStr prompt
                     hFlush stdout
                     line <- getLine
                     return (Just line)

addHistory stmt = return ""
-}
