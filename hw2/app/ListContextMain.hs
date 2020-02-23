import System.IO
import ListContext

helpMenu :: String
helpMenu = "Possible Operations:\n--------------------\n  l\tMove left\n" ++
           "  r\tMove right\n  e n\tEdit the current value to be the integer n\n"++
           "  i n\tInsert the integer n\n  d\tDelete the current value\n  q\tQuit the program\n"++
           "  h\tPrint this help message\n"

main :: IO ()
main = do
  putStrLn helpMenu
  aux initContext
  where headMay xs = case xs of
            (x:_) -> Just x
            []    -> Nothing
        isSpace c = c `elem` "\t \n\r\v\f"
        aux ctx = do
            putStr "-- Current list: "
            putStr $ show (contextToList ctx)
            case getCurItem ctx of
                 Just el -> do putStr " at element: "
                               print el
                 Nothing -> putStrLn ""
            putStr ("-- Enter an operation on the list: ")
            hFlush stdout
            input <- getLine
            case headMay input of
                Just 'l' -> aux (moveLeftL ctx)
                Just 'r' -> aux (moveRightL ctx)
                Just 'e' -> let w2 = dropWhile isSpace (tail input)
                            in aux (updateItem w2 ctx)
                Just 'i' -> let w2 = dropWhile isSpace (tail input)
                            in aux (insertItem w2 ctx)
                Just 'd' -> aux (deleteItem ctx)
                Just 'q' -> return ()
                Just 'h' -> putStrLn helpMenu >> aux ctx
                _ -> do putStrLn "!! Error: unrecognized command"
                        aux ctx
