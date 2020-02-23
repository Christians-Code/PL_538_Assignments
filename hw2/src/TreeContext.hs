module TreeContext where

-- CS 538, Spring 2020: Homework 2
-- Part 3: Tree contexts

-- Compilation instructions:
-- -------------------------
-- To run this file with cabal, execute the following command:
--
-- $ cabal v2-run treecontext

-- **Your code should compile without warnings.**
-- To treat all warnings as errors, build the program with following
-- cabal command:
--
-- $ cabal v2-build --ghc-options -Wall -Werror

-- We'll now extend contexts to work for trees. First, we'll set up a datatype
-- for plain trees. Trees will consist of Nodes, each with a piece of data of
-- type `a` and a list of trees which represent that Node's children.

data Tree a = Node a [Tree a]
  deriving (Eq)

-- Just like before, we want to represent a position in a datastructure as (1)
-- the context around the position, and (2) the data at the position. Both of
-- these pieces are more complicated for trees. Let's start with representing
-- contexts in a tree. We will represent contexts recursively: a context of a
-- node is either nothing, if the node is root of the tree, or it is the context
-- of its parent node, the parent node's data, and two lists describing the
-- node's siblings (child nodes of the same parent): the sibling trees before
-- the current node, and sibling trees after the current node.

data Context a =
    Empty
  | Loc a [Tree a] (Context a) [Tree a]

-- Just like we did for list contexts, we will store the list of siblings before
-- the current position in *reverse* order, and the list of siblings after the
-- current position in *normal* order. Keep this invariant in mind and make sure
-- that it is preserved in all the operations.

-- Since the position may be an internal node, the item at a position may be a
-- tree. By combining these two pieces of data, we are ready to define the type
-- of tree contexts.

data TreeContext a = TC { getContext :: Context a, getItem :: Tree a }

-- Write a function to convert a regular tree into a context, with the initial
-- position set to the root.

contextOfTree :: Tree a -> TreeContext a
contextOfTree = undefined

-- Write a function to convert a context back into a regular tree.
-- (Hint: you'll want to recurse, calling contextToTree on a parent context.)

contextToTree :: TreeContext a -> Tree a
contextToTree = undefined

-- Write a function to get the subtree at the current position.

getCurTree :: TreeContext a -> Tree a
getCurTree = undefined

-- For trees, we can move the position in more directions. A left/right move
-- corresponds to switching to one of the siblings of the current position,
-- while an up/down move corresponds to moving to the parent or moving to the
-- first child of the current position.
--
-- If the move is not valid (trying to move left at the first sibling, or trying
-- to move right in the last sibling, etc.), return the original context with no
-- change. Your function should be total---it should never raise an error!

moveLeftT :: TreeContext a -> TreeContext a
moveLeftT = undefined

moveRightT :: TreeContext a -> TreeContext a
moveRightT = undefined

moveUpT :: TreeContext a -> TreeContext a
moveUpT = undefined

moveDownT :: TreeContext a -> TreeContext a
moveDownT = undefined

-- Write a function to replace the tree at the current position in the context
-- with a new subtree. Again, your function should never raise an error!

updateT :: Tree a -> TreeContext a -> TreeContext a
updateT = undefined

-- Write a function to insert a new subtree into the context. There are three
-- possible places to insert: left (as a new sibling before the current
-- position), right (as a new sibling after the current position), and down (as
-- a new first/left-most child of the current position). The cursor should end
-- up on the newly inserted item. If the insertion is invalid (inserting
-- left/right at the root), return the original context. Again, your function
-- should never raise an error!

insertLeftT :: Tree a -> TreeContext a -> TreeContext a
insertLeftT = undefined

insertRightT :: Tree a -> TreeContext a -> TreeContext a
insertRightT = undefined

insertDownT :: Tree a -> TreeContext a -> TreeContext a
insertDownT = undefined

-- Write a function to delete the whole subtree at the current position from the
-- context. The final position of the context should be (in decreasing priority)
-- the next sibling on the right, or the previous sibling on the left, or the
-- parent node if there are no other siblings.
--
-- Again, your function should behave correctly for all contexts; deleting the
-- root node should return the original context.

deleteT :: TreeContext a -> TreeContext a
deleteT = undefined

-- If you would like to change the starting context that the main function of
-- this program, change the definition below.

initContext :: TreeContext Integer
initContext = contextOfTree initTree

initTree :: Tree Integer
initTree = Node 1 [Node 2 [], Node 3 []]
