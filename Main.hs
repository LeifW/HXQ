{--------------------------------------------------------------
-
- The main program of the XQuery interpreter
- Programmer: Leonidas Fegaras (fegaras@cse.uta.edu)
- Date: 01/13/2009
-
---------------------------------------------------------------}

{-# OPTIONS -cpp #-}

module Main where

#if __GLASGOW_HASKELL__ >= 609
import qualified Control.OldException as C
#else
import qualified Control.Exception as C
#endif
import List(sort)
import System.Environment
import System.CPUTime
import Text.XML.HXQ.XQuery
import Text.XML.HXQ.Functions
import Text.XML.HXQ.Types
import Text.XML.HXQ.Interpreter(evalInput,xqueryE,xfileDB)


type E = C.Exception


version = "0.16.0"


parseEnv :: [String] -> [(String,String)]
parseEnv [] = [("o","Temp.hs")]
parseEnv ("-help":xs) = ("help",""):(parseEnv xs)
parseEnv ("-c":file:xs) = ("c",file):(parseEnv xs)
parseEnv ("-o":file:xs) = ("o",file):(parseEnv xs)
parseEnv ("-db":file:xs) = ("db",file):(parseEnv xs)
parseEnv ("-v":xs) = ("v",""):(parseEnv xs)
parseEnv ("-t":xs) = ("t",""):(parseEnv xs)
parseEnv ("-p":query:file:xs) = ("p","doc('"++file++"')"++query):(parseEnv xs)
parseEnv (('-':x):_) = error ("Unrecognized option -"++x++". Use -help.")
parseEnv (file:xs) = ("r",file):(parseEnv xs)


noDBerror = error "Missing Database Connection; use the option -db in xquery"


main = do senv <- getArgs
          let env = parseEnv senv
              verbose = case lookup "v" env of Nothing -> False; _ -> True
              timing = case lookup "t" env of Nothing -> False; _ -> True
              putTime t = if timing then putStrLn $ "Evaluation time: "++show (div t (10^9))++" milliseconds" else return ()
          case lookup "help" env of
            Just _ -> do putStrLn ("HXQ: XQuery Interpreter version "++version)
                         putStrLn "The documentation is availabe at http://lambda.uta.edu/HXQ/"
                         putStrLn "Command line options and files:"
                         putStrLn "   xquery-file               evaluate the XQuery in xquery-file using the interpreter"
                         putStrLn "   -db database              use the relational database during querying"
                         putStrLn "   -c xquery-file            compile the XQuery in xquery-file into Haskell code"
                         putStrLn "   -o haskell-file           set the Haskell file for -c (default is Temp.hs)"
                         putStrLn "   -p XPath-query xml-file   interpret the XPath query against the xml-file"
                         putStrLn "   -v                        print verbose information (AST and optimized plan)"
                         putStrLn "   -t                        print timing information"
                         putStrLn "Without an xquery-file, it reads and evaluates the input using the HXQ interpreter."
                         putStrLn "   The input may be an XQuery or a 'declare variable' or a 'declare function'."
                         putStrLn "   To write an XQuery in multiple lines, wrap it in {}."
                         putStrLn $ "Functions (name/arity):  " ++ (unwords $ sort $ map (\(f,c,_,_) -> f++"/"++show c) systemFunctions)
                         putStrLn $ "Path Steps:  " ++ (unwords $ map fst pathFunctions)
                         putStrLn $ "Build-in Types:  xs:anyAtomicType " ++ (unwords $ map fst buildInTypes)
            _ -> case lookup "c" env of
                   Just file -> do query <- readFile file
                                   let qf = foldr (\c r -> if c=='\"' then '\\':c:r else c:r)
                                                  "" (foldr1 (\a r -> a++" "++r) (lines query))
                                       db = case lookup "db" env of Just filepath -> filepath; _ -> ""
                                       pr = "{-# LANGUAGE TemplateHaskell #-}\nmodule Main where\nimport Text.XML.HXQ.XQuery\n\nmain = do "
                                            ++ (if db=="" then "res <- " else "db <- connect \""++db++"\"\n          res <- ")
                                            ++ (if db=="" then "$(xq \"" else "$(xqdb \"")
                                            ++ qf ++ "\")"++(if db=="" then "" else " db")++"\n          putXSeq res\n"
                                       Just ofile = lookup "o" env
                                   writeFile ofile pr
                   _ -> case lookup "r" env of
                          Just file -> case lookup "db" env of
                                         Just filepath -> do db <- connect filepath
                                                             t1 <- getCPUTime
                                                             result <- xfileDB file db
                                                             putXSeq result
                                                             t2 <- getCPUTime
                                                             putTime (t2-t1)
                                                             commit db
                                         _ -> do query <- readFile file
                                                 t1 <- getCPUTime
                                                 (result,_,_) <- xqueryE query [] [] noDBerror verbose
                                                 putXSeq result
                                                 t2 <- getCPUTime
                                                 putTime (t2-t1)
                          _ -> case lookup "p" env of
                                 Just query -> do t1 <- getCPUTime
                                                  (result,_,_) <- xqueryE query [] [] noDBerror verbose
                                                  putXSeq result
                                                  t2 <- getCPUTime
                                                  putTime (t2-t1)
                                 _ -> do putStrLn ("HXQ: XQuery Interpreter version "++version++". Use -help for help.")
                                         case lookup "db" env of
                                           Just filepath
                                               -> do db <- connect filepath
                                                     evalInput (\s vs fs -> C.catch
                                                                          (do t1 <- getCPUTime
                                                                              (result,nvs,nfs) <- xqueryE s vs fs db verbose
                                                                              putXSeq result
                                                                              t2 <- getCPUTime
                                                                              putTime (t2-t1)
                                                                              commit db
                                                                              return (nvs,nfs))
                                                                          (\e -> do putStrLn (show (e::E)); return (vs,fs))) [] [] "> "
                                           _ -> evalInput (\s vs fs-> C.catch
                                                                      (do t1 <- getCPUTime
                                                                          (result,nvs,nfs) <- xqueryE s vs fs noDBerror verbose
                                                                          putXSeq result
                                                                          t2 <- getCPUTime
                                                                          putTime (t2-t1)
                                                                          return (nvs,nfs))
                                                                      (\e -> do putStrLn (show (e::E)); return (vs,fs))) [] [] "> "
