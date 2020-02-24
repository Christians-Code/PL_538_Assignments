import Data.Char
import System.IO

import TreeContext

helpMenu :: String
helpMenu = "Possible Operations:\n--------------------\n  l\tMove left\n" ++
           "  r\tMove right\n  u\tMove up\n  d\tMove down\n" ++
           "  e n\tEdit the current value to be the integer n\n" ++
           "  il n\tInsert the integer n to the left\n  ir n\tInsert the " ++
           "integer n to the right\n  id n\tInsert the integer n below\n"++
           "  del\tDelete the current value\n  q\tQuit the program\n"++
           "  h\tPrint this help message"

updateTree :: a -> Tree a -> Tree a
updateTree val (Node _ ts) = Node val ts

drawTree :: Tree String -> String
drawTree = unlines . draw

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

contextToStringTree :: TreeContext Integer -> Tree String
contextToStringTree (TC c (Node v children)) = kernel c (Node ("(" ++ show v ++ ")") (convertTrees children))
  where kernel :: Context Integer -> Tree String -> Tree String
        kernel Empty t = t
        kernel (Loc pv l p r) t = kernel p (Node (show pv) (convertTrees (reverse l) ++ t:convertTrees r))

convertTrees :: [Tree Integer] -> [Tree String]
convertTrees = map convert
  where convert :: Tree Integer -> Tree String
        convert (Node v children) = Node (show v) (convertTrees children)

main :: IO ()
main = do
  putStrLn helpMenu
  aux initContext
    where checkArg :: [String] -> Either String Integer
          checkArg [] = Left "!! Error: command requires a second argument"
          checkArg [i]
            | all isDigit i = Right $ read i
            | otherwise     = Left "!! Error: second argument must be an integer"
          checkArg _ = Left "!! Error: only one argument is accepted"

          aux :: TreeContext Integer -> IO ()
          aux ctx = do
            putStrLn "\n-- Current Tree:\n"
            putStrLn $ drawTree $ contextToStringTree ctx
            putStr "-- Enter an operation on the list: "
            hFlush stdout
            cmd <- getLine
            case words cmd of
              ("l":_) -> aux (moveLeftT ctx)
              ("r":_) -> aux (moveRightT ctx)
              ("u":_) -> aux (moveUpT ctx)
              ("d":_) -> aux (moveDownT ctx)
              ("del":_) -> aux (deleteT ctx)
              ("e":arg) -> case checkArg arg of
                             Left s -> putStrLn s >> aux ctx
                             Right i -> aux (updateT (updateTree i (getCurTree ctx)) ctx)
              ("il":arg) -> case checkArg arg of
                              Left s -> putStrLn s >> aux ctx
                              Right i -> aux (insertLeftT (Node i []) ctx)
              ("ir":arg) -> case checkArg arg of
                              Left s -> putStrLn s >> aux ctx
                              Right i -> aux (insertRightT (Node i []) ctx)
              ("id":arg) -> case checkArg arg of
                              Left s -> putStrLn s >> aux ctx
                              Right i -> aux (insertDownT (Node i []) ctx)
              ("q":_) -> return ()
              ("h":_) -> putStrLn helpMenu >> aux ctx
              _ -> putStrLn "!! Error: unrecognized command" >> aux ctx
