module Duodoku where

-- CS 538, Spring 2020: Homework 1
-- Name: Vinay Patil
-- Email: vpatil3@wisc.edu

-- Your code should compile **without warnings**.
-- The following line makes the compiler treat all warnings as hard errors.
-- When you are done, uncomment it and fix until there are no more errors.
{-# OPTIONS -Wall -Werror #-}
--
-- You will want some of the functions from this library:

import Data.List

-- Please **do not change** the names and type signatures for the functions
-- below: we will use these names when testing your assignment. Of course, you
-- can introduce new functions if you want.

-- Everyone knows Sudoku, the combinatorial number-placement puzzle. But Sudoku
-- is old news... let's do Duodoku! The objective is to fill a 16x16 grid with
-- the digits 1-8 so that each column, each row, and each of the 16 4x4 subgrids
-- that compose the 16x16 grid contain each of the digits 1-8 exactly *twice*.
--
-- You will develop a solver which will take a Duodoku puzzle and return the
-- solved puzzle.  We will walk you through a common technique in functional
-- programming where a complex task is broken into smaller parts, and then
-- composed together.
-- 
-- To run the program, execute the command:
-- ```
-- cabal v2-run
-- ```
-- And provide one of the puzzles as input (the full path is required). See the
-- puzzles/ directory for some sample puzzles.
--
-- To submit your homework, execute the command:
-- ```
-- cabal v2-sdist -o .
-- ```
-- which will generate a tarball titled
--
-- hw1-0.1.0.0.tar.gz
--
-- in your current working directory. Upload this tarball to Canvas.

-- Basic Declarations:
-- -------------------

type Grid = Matrix Char
type Matrix a = [Row a]
type Row a = [a]

-- Basic Definitions:
-- ------------------

-- The size of a box.
boxsize :: Int
boxsize = 4

-- The possible values that a Duodoku puzzle can contain.
values :: [Char]
values = ['1'..'8']

-- "empty" returns true if the character is a '.', false otherwise.
-- Note: The '.' character denotes an empty field in a grid.
empty :: Char -> Bool
empty = (== '.')

-- "single" returns true if the list is a singleton list, false otherwise.
single :: [a] -> Bool
single [_] = True
single _   = False

-- Extracting rows, columns, and boxes:
-- ------------------------------------
--
-- Since each row, column, and box has to have each of the digits 1-8 twice,
-- we have to be able to extract the rows, columns, and boxes from a grid
-- in order to check them. Implement the following two functions which
-- takes as an argument a matrix and returns all the rows and all of the columns,
-- respectively. We have provided "boxes" to you which returns all of the 4x4 subgrids
-- of a grid.
--
-- Hint: the "transpose" function from Data.List may be useful.

rows :: Matrix a -> [Row a]
rows a = a

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
  where
    pack   = split . map split
    split  = chop boxsize
    unpack = map concat . concat

chop                  :: Int -> [a] -> [[a]]
chop _ []             =  []
chop n xs             =  take n xs : chop n (drop n xs)

-- Validity checking:
-- ------------------
--
-- Now that we can extract the rows, columns, and boxes, we can verify that
-- a grid is *valid*. A grid is *valid* if and only if there are no more than two copies
-- of the same value in any row, column, or box. For example, if a row of a grid
-- had three '7's, it would not be a valid grid. However, if all rows, columns, and
-- boxes had at maximum two '1's, two '2's, ..., two '8's, then it would be *valid*.
--
-- Implement the "max2dups" function which checks that a list of characters has
-- *at most* two copies of each value. For example,
--
-- max2dups ['1', '1', '2'] = True
-- max2dups ['1', '2', '1', '3', '1'] = False
-- max2dups ['1'] = True
--
-- To do this, first remove one occurrence of the first element of the list, 'x'
-- from rest of the list, 'xs'. Then, check to see if 'x' appears in the rest
-- of the list again. If it does, that means the list has at least three copies
-- of 'x' and thus "max2dups" should return False. Then, check to make sure
-- that the remaining elements of the list also do not appear twice using recursion.
-- 
-- Hint: The (\\) function from Data.List may be useful.

max2dups :: [Char] -> Bool
max2dups [] = True
max2dups (x:xs) = (x `notElem` newlist) && max2dups newlist
  where newlist = xs \\ [x]
                

-- Using "max2dups", implement the "valid" function which returns true
-- if the given grid is valid, and false otherwise. Note that a grid is not
-- valid if max2dups returns "False" for any row, column, or box.
--
-- Hint: The "all" function from Data.List may be useful.

valid :: Grid -> Bool
valid inputGrid = all max2dups (rows inputGrid) && all max2dups (cols inputGrid) && all max2dups (boxes inputGrid) 

-- A Naive Attempt:
-- ----------------
--
-- A simple but extremely inefficient way to solve a Duodoku puzzle is to
-- enumerate every possible completed grid and search for a valid grid in this
-- list.
--
-- To accomplish this, you will first implement the function, "choices", which
-- takes a grid and returns a matrix of choices, where a matrix of choices is a
-- grid where each element is a list of possible values to go into that position.
-- We have two cases:
--
-- 1) If the position is *empty*, then the possible choices should be all digits from 1-8.
-- 2) If the position is not empty, then there is only one option, namely the digit that
-- is already there.
--
-- For example, if we have the grid (for simplicity, assume that values = [1..3]
-- our grid is a 6x6 instead of a 16x16):
--
-- 1 . . 3 . 2
-- . 2 . 3 . 1
-- . . . . . .
-- . 1 2 . 3 .
-- . . . 1 . .
-- . 2 . . . .
--
-- then if we apply "choices" to that grid we should get
--
-- [1]     [1,2,3] [1,2,3] [3]     [1,2,3] [2]
-- [1,2,3] [2]     [1,2,3] [3]     [1,2,3] [1]
-- [1,2,3] [1,2,3] [1,2,3] [1,2,3] [1,2,3] [1,2,3]
-- [1,2,3] [1]     [2]     [1,2,3] [3]     [1,2,3]
-- [1,2,3] [1,2,3] [1,2,3] 1       [1,2,3] [1,2,3]
-- [1,2,3] [2]     [1,2,3] [1,2,3] [1,2,3] [1,2,3]
--
-- We are not concerned at this point about whether the choices are valid, just
-- what values could possibly be plugged in (namely "values"). You should not
-- extract any rows/columns/boxes from a grid, or check if rows/columns/boxes
-- are valid.
--
-- Hint: Use the "map" function.

mainMap = map (\x -> if empty x then values else [x])

choices :: Grid -> Matrix [Char]
choices = map mainMap

-- We now have a matrix of choices where each sequence of choices corresponds to
-- a unique grid. If we were to select one element from each list we would
-- have a completed, although not necessarily valid, grid. From the previous
-- matrix of choices we can generate a single grid by choosing the first
-- element of each list within the matrix.
--
-- [1]     [1,2,3] [1,2,3] [3]     [1,2,3] [2]
-- [1,2,3] [2]     [1,2,3] [3]     [1,2,3] [1]
-- [1,2,3] [1,2,3] [1,2,3] [1,2,3] [1,2,3] [1,2,3]
-- [1,2,3] [1]     [2]     [1,2,3] [3]     [1,2,3]
-- [1,2,3] [1,2,3] [1,2,3] 1       [1,2,3] [1,2,3]
-- [1,2,3] [2]     [1,2,3] [1,2,3] [1,2,3] [1,2,3]
--                        ||
--                        ||
--                        \/
--                   1 1 1 3 1 2
--                   1 2 1 3 1 1
--                   1 1 1 1 1 1
--                   1 1 2 1 3 1
--                   1 1 1 1 1 1
--                   1 2 1 1 1 1
--
-- More generally, we would like to generate all possible grids, not just the
-- first one. To do this, implement the following two functions: "cartProd" and
-- "expandAll".
--
-- "cartProd" is the Cartesian product of a list of lists, generalizing the
-- Cartesian product of two lists. For example, cartProd [[1,2],[3,4],[5,6]]
-- gives:

--   [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
--
-- Hint: you'll want to use recursion on the input list. The cases are an empty
-- list (cartProd []), and a non-empty list (cartProd (xs:xss))

cartProd :: [[a]] -> [[a]]
cartProd [] = [[]]
cartProd (xs:xss) = concatMap (\each -> map (each:) (cartProd xss)) xs
-- "expandAll" will transform a matrix of choices into a list of matrices of choices.
-- In other words, it will take the matrix that "choices" provides us, and will
-- enumerate all possible grids which result from making a selection from each
-- list of that matrix. From our previous example,
--
--                   1 1 1 3 1 2
--                   1 2 1 3 1 1
--                   1 1 1 1 1 1
--                   1 1 2 1 3 1
--                   1 1 1 1 1 1
--                   1 2 1 1 1 1
--
-- should be an element of the list that "expandAll" returns if we gave it the
-- following matrix:
--
-- [1]     [1,2,3] [1,2,3] [3]     [1,2,3] [2]
-- [1,2,3] [2]     [1,2,3] [3]     [1,2,3] [1]
-- [1,2,3] [1,2,3] [1,2,3] [1,2,3] [1,2,3] [1,2,3]
-- [1,2,3] [1]     [2]     [1,2,3] [3]     [1,2,3]
-- [1,2,3] [1,2,3] [1,2,3] 1       [1,2,3] [1,2,3]
-- [1,2,3] [2]     [1,2,3] [1,2,3] [1,2,3] [1,2,3]
--
-- Again, we aren't concerned about whether a grid is valid, just that
-- it is a possible grid.
--
-- Hint: you'll want to use cartProd twice: first on rows, and then on columns...

expandAll :: Matrix [a] -> [Matrix a]
expandAll  mat = cartProd (map cartProd mat)

-- At this point, we have what we need to solve a Duodoku puzzle, albeit slowly.
-- "slowSolve" takes as input a grid and will solve the puzzle by returning a
-- list of all possible solutions to the puzzle.
--
-- Implement "slowSolve" using the following algorithm:
-- 1) Generate the matrix of choices for the grid.
-- 2) Expand all of these choices into a list of all of the possible grids.
-- 3) Filter out all of the invalid ones.

slowSolve :: Grid -> [Grid]
slowSolve inputGrid = createNew(expandAll(choices inputGrid))
  where createNew [] = []
        createNew (x:xs) = if valid x
                            then x : createNew xs
                            else createNew xs

-- Try out "slowSolve" on easy.puz and medium.puz. I wouldn't recommend trying it
-- on hard.puz...

-- Pruning the search space:
-- -------------------------
-- 
-- You might have noticed that "slowSolve" is, well, slow. We will now try and
-- improve this with a smart search strategy: rather than expanding all choices
-- at once, we will expand one square, prune out invalid grids, then continue
-- expanding. The hope is that we will prune out invalid grids earlier in the
-- process, reducing the search space.
-- 
-- Consider the following row of a sample grid (for simplicity, assume that
-- values = [1..3] and our grid is a 6x6 instead of a 16x16):
-- 
-- [1] [1,2,3] [1,2,3] [3] [1,2,3] [1]
--
-- Note that the singleton list [1] appears twice in this row. Thus, we have
-- only one choice for the first and sixth places of this row, namely the digit
-- '1'. Additionally, the digit '3' has to appear in the fourth position of this
-- row. Therefore, in order for this row to be valid, '1' cannot appear for a
-- third time. So, we should eliminate '1' from the remaining positions. We
-- will call this action *pruning*. After pruning the above row we would get
--
-- [1] [2,3] [2,3] [3] [2,3] [1]
--
-- Note that we do not remove '3' as a choice from the remaining positions as we
-- don't know yet where both '3's should be placed.
--
-- Likewise, we can apply pruning on columns and boxes.
--
-- We will build up the function "prune" using four helper functions, three of
-- which you will implement. First, implement "minus" which takes two lists of
-- characters and will remove the first occurrence of each element of the second
-- list from the first list *unless* the first list is a singleton list. For
-- example,
--
-- minus ['1','2','3'] ['2','1'] = ['3']
-- minus ['1'] ['1','2','3']     = ['1']

removeFirst :: [Char] -> Char -> [Char]
removeFirst [] _ = []
removeFirst (x:xs) y = if x == y then xs else x : removeFirst xs y

minus :: [Char] -> [Char] -> [Char]
minus a b = if single a
              then a
              else loopb a b
  where loopb x [] = x 
        loopb x (y:ys) = loopb (removeFirst x y) ys

-- "getDups" takes in a list and will return a list containing all of the
-- elements that are duplicated *at least* twice. So, for example,
--
-- getDups [1,2,3]       = []
-- getDups [1,2,1,3,4,3] = [1,3]

getDups :: Ord a => [a] -> [a]
getDups = map head . filter (\l -> length l > 1) . group . sort

-- Next, "getSingles" will take in a row of choices and will return the list of
-- characters that appear at least twice as *singleton* lists. So, for example,
--
-- getSingles [[1],[1,2,3],[1,2,3],[3],[1,2,3],[1]] = [1]
-- getSingles [[2],[2],[1],[1],[1,3],[1,2,3]]       = [1,2]
--
-- Implement "getSingles".
--
-- Hint: you'll probably want to use "getDups"; the "concat" function in
-- Data.List may be useful.

getSingles :: Row [Char] -> [Char]
getSingles inputRow = helper inputRow []
  where helper [] acc = getDups acc
        helper (x:xs) acc = if single x
                              then helper xs (acc ++ x)
                              else helper xs acc
 

-- Using "getSingles" and "minus", implement the function "reduce" which does
-- the actual action of "pruning", meaning, for each list of choices within a
-- row, subtract the digits that have been repeated twice already. So, for
-- example,
--
-- reduce [[1],[1,2,3],[1,2,3],[3],[1,2,3],[1]] = [[1],[2,3],[2,3],[3],[2,3],[1]]
--
-- Because the singleton list [1] appears twice in the row, we remove every
-- other occurrence of '1' in the row.

reduce :: Row [Char] -> Row [Char]
reduce inputRow = helper (getSingles inputRow) inputRow []
  where helper _ [] acc = acc
        helper singles (y:ys) acc = helper singles ys (acc ++ [minus y singles])   

-- "prune" applies "reduce" to all of the rows, columns, and boxes within the
-- grid. For those paying more attention, note that this works because:
--
-- rows . rows   = id
-- cols . cols   = id
-- boxes . boxes = id

prune :: Matrix [Char] -> Matrix [Char]
prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map reduce . f

-- Properties of Matrices:
-- -----------------------
--
-- Now, instead of enumerating all possible grids, we can first repeatedly prune
-- the matrix of choices down so that we don't generate invalid grids.
-- Unfortunately, this is still not enough to solve a Duodoku puzzle in
-- efficient time---we need a better pruning strategy.
--
-- You'll implement a series of predicates to check if a matrix of choices can
-- lead to a feasible solution. You'll want to take a quick look at the library
-- functions here:
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#g:13
--
-- Complete: A matrix of choices is complete if each square contains a single choice.
--
-- For example,
--
-- [1] [2] [1] [3] [2] [3]
-- [1] [1] [3] [2] [2] [3]
-- [3] [2] [3] [2] [1] [1]
-- [2] [3] [1] [3] [1] [2]
-- [3] [1] [2] [1] [3] [2]
-- [2] [3] [2] [1] [3] [1]
--
-- is complete but
--
-- [1] [2]   [1] [3] [2] [3]
-- [1] [1,2] [3] [2] [2] [3]
-- [3] [1,2] [3] [2] [1] [1]
-- [2] [3]   [1] [3] [1] [2]
-- [3] [1]   [2] [1] [3] [2]
-- [2] [3]   [2] [1] [3] [1]
--
-- is not since there are still two choices to be made.

complete :: Matrix [Char] -> Bool
complete inputMatrix = helper inputMatrix True
  where helper [] acc = acc
        helper _ False = False
        helper (x:xs) acc =  helper xs acc && all single x
-- Void: A matrix of choices is void if some position contains no choices.
--
-- For example,
--
-- []  [2] [1] [3] [2] [3]
-- [1] [1] [3] [2] [2] [3]
-- [3] []  [3] [2] [1] [1]
-- [2] [3] [1] [3] [1] [2]
-- [3] [1] []  [1] [3] [2]
-- [2] [3] [2] [1] [3] [1]
--
-- is void.

void :: Matrix [Char] -> Bool
void inputMatrix = helper inputMatrix False
  where helper [] acc = acc
        helper _ True = True
        helper (x:xs) acc =  helper xs acc || any null x

-- Consistent: A *row of choices* is consistent if the row does not contain more
--             than two occurrences of the same single choice.
-- 
-- An example of a consistent row is
--
-- [1] [2] [1,3] [3] [1,3] [2]
--
-- whereas an example of an inconsistent row is
--
-- [1] [2,3] [1] [3] [1] [2,3].

checkConsistency :: [Char] -> [Char]
checkConsistency = map head . filter (\l -> length l > 2) . group . sort

consistent :: Row [Char] -> Bool
consistent inputRow = helper inputRow []
  where helper [] acc = null (checkConsistency acc)
        helper (x:xs) acc = if single x
                            then helper xs (acc ++ x)
                            else helper xs acc

-- Safe: A matrix of choices is safe if all rows/columns/boxes are consistent.

safe :: Matrix [Char] -> Bool
safe inputMatrix = all consistent (rows inputMatrix) && all consistent (cols inputMatrix) && all consistent (boxes inputMatrix)

-- Blocked: A matrix of choices is blocked if it is void or *not* safe.

blocked :: Matrix [Char] -> Bool
blocked inputMatrix = void inputMatrix || not (safe inputMatrix)

-- Making Choices... One at a Time:
-- --------------------------------
--
-- Clearly, a blocked matrix cannot lead to a solution. If we use "expandAll"
-- from before, a blocked matrix will be needlessly expanded many times.
--
-- This problem can be addressed by expanding choices one square at a time, and
-- filtering out any resulting matrices that are blocked before considering any
-- further choices.
--
-- "expand" behaves in the same way as "expandAll", except that it only expands
-- the first square with more than one choice:

expand :: Matrix [Char] -> [Matrix [Char]]
expand m =
    [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
       (rows1,row:rows2) = break (any (not . single)) m
       (row1,cs:row2)    = span single row

-- Using "expand", implement the function "search" that makes one choice at a time
-- from a matrix of choices and returns a list of possible solutions.
--
-- The overall logic is as follows. A matrix is in one of three possible states:
-- 1) The matrix is blocked: There are no possible solutions.
-- 2) The matrix is complete: No choices need to be made
--    and so "expandAll" should be used to convert it into a grid.
-- 3) The matrix is not blocked and not complete: The matrix can be further
--    simplified. The matrix should first be expanded once using "expand",
--    pruned using "prune", and then "search" should continue recursively.
--
-- Hint: One way of doing the third case is by using "concat".

search :: Matrix [Char] -> [Grid]
search inputMatrix
    | blocked inputMatrix = []
    | complete inputMatrix = expandAll inputMatrix
    | otherwise = helper (map prune (expand inputMatrix)) []
    where
        helper [] acc = acc
        helper (x : xs) acc = helper xs (acc ++ search x)

-- Lastly, implement the "fastSolve" function which takes in a grid, generates
-- choices, and then searches for possible solutions.
--
-- When you're finished, try out your solver! While it is not much faster than
-- the slow solver on easy.puz, you'll notice that it's much faster on
-- medium.puz, and it will actually solve hard.puz without exhausting all of
-- your machine's memory.

fastSolve :: Grid -> [Grid]
fastSolve inputGrid = search (choices inputGrid)