module ListContext where

-- CS 538, Spring 2020: Homework 2
-- Part 2: List contexts

-- Compilation instructions:
-- -------------------------
-- To run this file with cabal, execute the following command:
--
-- $ cabal v2-run listcontext

-- **Your code should compile without warnings.**
-- To treat all warnings as errors, build the program with following
-- cabal command:
--
-- $ cabal v2-build --ghc-options -Wall -Werror

-- A context takes a plain datastructure---a list, a tree, etc.---and adds an
-- extra bit of information describing a position within the datastructure. This
-- position behaves much like a cursor: we can move the cursor, lookup the value
-- at the cursor, or update/delete the value at the cursor. 

-- We can represent a position in a non-empty plain list with three parts: the
-- list of elements before the position, the element at the lambdaposition, and the
-- list of elements after the position. Otherwise, the context may be over an
-- empty list. The elements of the list will be of type `a`. 

data ListContext a =
    LCEmpty
  | LCItems [a]   -- list of items before
            a     -- current item
            [a]   -- list of items after

-- One detail about Haskell lists is that they support efficient operations at
-- only one end, the start (head) of the list. Working at the tail end
-- requires traversing the whole list, an expensive operation.
-- 
-- To make the context operations easier to write and more efficient, we
-- store the list of items before in *reversed* order. For instance, if the
-- underlying list is [1, 2, 3, 4, 5] and the current position is 3, then the
-- context should look like
--
-- LCItems [2, 1] 3 [4, 5]
--
-- Keep this invariant in mind, and make sure it is maintained through all of
-- the operations.

-- Write a function to convert a regular list into a context, with the initial
-- position set to the first element (if the list is non-empty).

contextOfList :: [a] -> ListContext a
contextOfList [] = LCEmpty
contextOfList list = LCItems [] (head list) (drop 1 list)

-- Write a function to convert a context back into a regular list.

contextToList :: ListContext a -> [a]
contextToList (LCEmpty) = []
contextToList (LCItems before current after) = (reverse before) ++ [current] ++ after

-- Write a function to get the current item. Note that the function returns
-- `Maybe a`. Your function should return `Nothing` if the context is empty.

getCurItem :: ListContext a -> Maybe a
getCurItem (LCEmpty) = Nothing
getCurItem (LCItems _ current _) = Just current

-- Write functions to move the position left or right in the context. Since we
-- cannot actually change the input context in a pure functional language, your
-- function will return a context with the updated position. Remember to keep
-- track of the elements in the context: moving left or right should not change
-- the elements in the underlying list.
--
-- If the input context is empty, or the move is not valid (trying to move left
-- in the first position, or trying to move right in the last position), return
-- the original context with no change. Your function should be total---it should
-- never raise an error!

moveLeftL :: ListContext a -> ListContext a
moveLeftL (LCEmpty) = LCEmpty
moveLeftL (LCItems [] current after) = LCItems [] current after
moveLeftL (LCItems (x:xs) current after) = LCItems xs x (current:after)

moveRightL :: ListContext a -> ListContext a
moveRightL (LCEmpty) = LCEmpty
moveRightL (LCItems before current []) = LCItems before current []
moveRightL (LCItems before current (x:xs)) = LCItems (current:before) x xs

-- Write a function to replace the item at the current position in the context
-- with a new element. If the input context is empty, return the input context
-- unchanged. Again, your function should never raise an error!

updateItem :: a -> ListContext a -> ListContext a
updateItem element (LCEmpty) = LCEmpty
updateItem element (LCItems before current after) = LCItems before element after

-- Write a function to insert a new element into the context *after* the current
-- position. The cursor should move to the new item after the insertion. Your
-- function should behave correctly for all contexts, including the empty context.

insertItem :: a -> ListContext a -> ListContext a
insertItem element (LCEmpty) = contextOfList [element]
insertItem element (LCItems before current after) = moveRightL (LCItems before current (element:after))

-- Write a function to delete the current element from the context. The cursor
-- should move to the item before the deleted position, or after the first
-- position when deleting the first item. Again, your function should behave
-- correctly for all contexts, including the empty context. (Deleting from the
-- empty context should just return the empty context.)

deleteItem :: ListContext a -> ListContext a
deleteItem (LCEmpty) = LCEmpty
deleteItem (LCItems [] current []) = LCEmpty
deleteItem (LCItems [] current (x:xs)) = LCItems [] x xs
deleteItem (LCItems (x:xs) current after) = LCItems xs x after

-- The main function allows you to traverse a list. You enter commands to move
-- around the context and modify the list using it. If you want to start from
-- a different context, change the definition of "initContext" below.

initContext :: ListContext String
initContext = contextOfList []
