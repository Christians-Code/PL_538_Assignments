module AppendList where

-- CS 538, Spring 2020: Homework 2
-- Part 1: Append lists

-- Compilation instructions:
-- -------------------------
-- To run this file with cabal, execute the following command:
--
-- $ cabal v2-run appendlist

-- **Your code should compile without warnings.**
-- To treat all warnings as errors, build the program with following
-- cabal command:
--
-- $ cabal v2-build --ghc-options -Wall -Werror

-- In the functional programming style, we usually avoid mutating or updating
-- memory cells, preferring instead pure operations. Accordingly, data
-- structures in functional languages look rather different from the data
-- structures you might be used to. In this assignment, you will write a few
-- purely functional data structures.

-- Concatenating lists with `(++)` is an expensive operation. It is linear in
-- the length of the first argument, which must be "unravelled" and then added
-- back on one at a time. 
-- To improve this, let's consider AppendLists. These use higher-order functions
-- so that appending becomes a constant-time operation. The price is that the
-- other list operations are much less efficient.

-- The elements of the list will be of type `a`. 
newtype AppendList a = AList ([a] -> [a])

-- The empty list is represented using the identity function, by
-- AList (\x -> x)
empty :: AppendList a
empty = AList (\x -> x)

-- The list [3] is represented as AList (\x -> 3:x)
-- The list [1, 9, 4] will be represented as the function (\x -> 1 : 9: 4 : x).

-- Generally in Haskell, functions can be used before they are defined. However,
-- please do these exercises *in order*: only use earlier functions.  
--
-- Based on these examples, write a function that takes on argument and returns
-- the AppendList that corresponds to the list with just that argument as
-- element
singleton :: a -> AppendList a
singleton element = AList (\x -> element:x)

-- Write a function that converts an AppendList to the regular list that it
-- represents
toList :: AppendList a -> [a]
toList (AList f) = f []

-- Write a function that prepends an element to any AppendList, just like cons
-- (written : ) does for lists
alistCons :: a -> AppendList a -> AppendList a
alistCons element (AList f) = AList ((\f x -> element:f x) f)

-- Write a replication function which makes an AppendList by repeating the given
-- element for a given number of times (possibly zero times).
alistReplicate :: Int -> a -> AppendList a
alistReplicate 0 _ = empty
alistReplicate times element = AList (\x -> take times (repeat element))

-- Write a function to append two AppendLists together.
alistAppend :: AppendList a -> AppendList a -> AppendList a
alistAppend (AList f1) (AList f2) = AList (f1.f2)

-- Write a concatenation function which takes a list of AppendLists and turns
-- them into a single AppendList (preserving the order).
alistConcat :: [AppendList a] -> AppendList a
alistConcat [] = empty
alistConcat (x:xs) = foldl alistAppend x xs

-- Write a function that does the opposite, converting a normal list into an
-- AppendList
fromList :: [a] -> AppendList a
fromList [] = empty
fromList list = AList (\x -> list ++ x)

-- We'll round out our AppendList functions with a few more operations. For
-- these operations, you may convert the AppendList to a regular list, perform
-- the list operation there, and then convert back to an AppendList. This is
-- slow, but these operations are not naturally supported by AppendLists so we
-- have little choice.

-- Write a function that computes the head of an AppendList (your function may
-- fail when the AppendList is empty).
alistHead :: AppendList a -> a
alistHead append = head (toList append)

-- Write foldr and map functions for AppendLists, just like for Lists
alistFoldr :: (a -> b -> b) -> b -> AppendList a -> b
alistFoldr func element append = foldr func element (toList append)

alistMap :: (a -> b) -> AppendList a -> AppendList b
alistMap mapFun append = fromList (map mapFun (toList append))

-- Write a function that computes the tail of an AppendList.
alistTail :: AppendList a -> AppendList a
alistTail append = fromList (tail (toList append))

-- Basic tests: you should do more testing!
checkEq :: (Eq a, Show a) => a -> a -> String
checkEq expected got
    | expected == got = "PASS"
    | otherwise       = "FAIL Expected: " ++ show expected ++ " Got: " ++ show got

tests :: IO ()
tests = let myList  = [1, 3, 5, 7, 9] :: [Int]
            myList' = [8, 6, 4, 2, 0] :: [Int] in
          do putStr "toList/fromList: "
             putStrLn $ checkEq myList (toList . fromList $ myList)
  
             putStr "alistHead: "
             putStrLn $ checkEq (head myList') (alistHead . fromList $ myList')
  
             putStr "alistTail: "
             putStrLn $ checkEq (tail myList) (toList . alistTail . fromList $ myList)
  
             putStr "alistFoldr: "
             putStrLn $ checkEq (sum myList) (alistFoldr (+) 0 $ fromList myList)
  
             putStr "alistMap: "
             putStrLn $ checkEq (map (+ 1) myList') (toList (alistMap (+ 1) $ fromList myList'))
  
             putStr "alistAppend: "
             putStrLn $ checkEq (myList ++ myList') (toList $ alistAppend (fromList myList) (fromList myList'))
  
             putStr "alistConcat: "
             putStrLn $ checkEq (myList' ++ myList) (toList $ alistConcat [fromList myList', fromList myList])
  
             putStr "alistReplicate: "
             putStrLn $ checkEq ([42, 42, 42, 42, 42] :: [Int]) $ toList (alistReplicate 5 42)
