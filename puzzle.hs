module Puzzle (
  Space,
  RowCol,
  GenericPuzzle,
  Puzzle,
  fromList,
  row,
  col,
  block,
  rowColBlock,
  isSolved
) where

import Data.Array
import Data.List (sort, nub)
import Data.Maybe (catMaybes)

type Space = Maybe Int

type RowCol = (Int, Int)

type GenericPuzzle a = Array RowCol a
type Puzzle = GenericPuzzle Space

-- Creates a 9x9 sudoku puzzle from a list of spaces. The spaces should be
-- indexed by row and then column, i.e. [(1, 1), (1, 2), ... (2, 1), ...].
fromList :: [a] -> GenericPuzzle a
fromList = listArray ((0, 0), (8, 8))

-- Returns the contents of a given row of a puzzle.
row :: GenericPuzzle a -> Int -> Array RowCol a
row puzzle ri =
  array ((ri, 0), (ri, 8)) [((ri, ci), puzzle ! (ri, ci)) | ci <- [0..8]]

-- Returns the contents of a given column of a puzzle.
col :: GenericPuzzle a -> Int -> Array RowCol a
col puzzle ci =
  array ((0, ci), (8, ci)) [((ri, ci), puzzle ! (ri, ci)) | ri <- [0..8]]

-- Returns the contents of a given block of a puzzle.
block :: GenericPuzzle a -> RowCol -> Array RowCol a
block puzzle (blockRow, blockCol) =
  listArray blockBounds [puzzle ! index | index <- range blockBounds]
    where
      blockBounds = ((startRow, startCol), (startRow + 2, startCol + 2))
      startRow = blockRow * 3
      startCol = blockCol * 3

-- Get a list of the unique numbers in the row, column, and block that a space
-- is in.
rowColBlock :: Puzzle -> RowCol -> [Int]
rowColBlock puzzle (rowIndex, colIndex) =
  sort . nub . catMaybes . concat . map elems $ [r, c, b]
    where
      r = row puzzle rowIndex
      c = col puzzle colIndex
      b = block puzzle (rowIndex `div` 3, colIndex `div` 3)

-- Checks whether a row, column, or block is complete.
isComplete :: Array RowCol Space -> Bool
isComplete = (== map Just [1..9]) . sort . elems

-- Checks whether a puzzle is solved.
isSolved :: Puzzle -> Bool
isSolved puzzle = and . map isComplete $ concat [rows, cols, blocks]
  where
    rows = map (row puzzle) [0..8]
    cols = map (col puzzle) [0..8]
    blocks = map (block puzzle) $ range ((0, 0), (2, 2))
