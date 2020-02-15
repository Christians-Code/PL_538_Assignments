module Main where

import System.IO
import Duodoku

main :: IO ()
main = do
  putStrLn "-- What Duodoku puzzle would you like to solve?"
  putStr   "-- Enter filename: "
  hFlush stdout
  puzzle_path <- getLine
  putStrLn "-- Which solver would you like to use?"
  putStr   "-- Enter 's' for slow, 'f' for fast: "
  hFlush stdout
  solver <- getChar
  puzzle_text <- readFile puzzle_path
  putStrLn $ "-- Solving " ++ puzzle_path ++ "..."
  let solve = case solver of
              'f' -> fastSolve
              's' -> slowSolve
              _ -> error $ "Unrecognized character: Only 'f' and 's' are accepted."
      sols = solve $ lines puzzle_text in
    case sols of
      []    -> putStrLn "-- No solutions found!"
      (s:_) -> putStrLn $ "-- Done! Here's a solution:\n\n" ++ (unlines s)
