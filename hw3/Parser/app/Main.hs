module Main where

import Ruse.Eval
import Ruse.Parser
import Ruse.Syntax

import Data.Map.Strict as Map
import System.IO

main :: IO ()
main = do
  putStrLn "Welcome to the Ruse REPL! (Press Ctrl-D to quit.)"
  repl Map.empty

--
--
-- Main REPL
--
-- Complete the function here. The logic should be something like this:
--
-- 1) Read in a line
-- 2) Try to parse it as a definition, using parseDef
-- 3) If parseDef succeeds, put the returned name and definition into the map
-- 4) Otherwise, try to parse using parseRExpr
-- 5) If it succeeds, evaluate and print the result
-- 6) If it fails, report fail parse
-- 7) Go back to 1)
--
--
repl :: Globals -> IO ()
repl gctx = do
  putStr "RusEPL> "
  hFlush stdout
  input <- getLine
  case (parse (parseDef gctx []) input) of
    ParseOK (v, d) -> let new_gctx = insert v d gctx in
                        repl new_gctx
    ParseError e -> case (parse (parseRExpr gctx) input) of
                      ParseOK x -> case eval x of
                                    Left err -> putStrLn err
                                    Right x -> putStrLn (ppRExpr x)
                      ParseError e -> putStrLn "fail parse"
  
  repl gctx
  

