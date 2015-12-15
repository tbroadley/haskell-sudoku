module Puzzle (
  Space,
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

type Puzzle = Array (Int, Int) Space

-- Creates a 9x9 sudoku puzzle from a list of spaces. The spaces should be
-- indexed by row and then column, i.e. [(1, 1), (1, 2), ... (2, 1), ...].
fromList :: [a] -> Array (Int, Int) a
fromList = listArray ((0, 0), (8, 8))

-- Returns the contents of a given row of a puzzle.
row :: Puzzle -> Int -> Array Int Space
row puzzle rowIndex =
  listArray (0, 8) [puzzle ! (rowIndex, colIndex) | colIndex <- [0..8]]

-- Returns the contents of a given column of a puzzle.
col :: Puzzle -> Int -> Array Int Space
col puzzle colIndex =
  listArray (0, 8) [puzzle ! (rowIndex, colIndex) | rowIndex <- [0..8]]

-- Returns the contents of a given block of a puzzle.
block :: Puzzle -> (Int, Int) -> Array Int Space
block puzzle (blockRow, blockCol) =
  listArray (0, 8) [puzzle ! index | index <- range blockBounds]
    where
      blockBounds = ((startRow, startCol), (startRow + 2, startCol + 2))
      startRow = blockRow * 3
      startCol = blockCol * 3

-- Get a list of the unique numbers in the row, column, and block that a space
-- is in.
rowColBlock :: Puzzle -> (Int, Int) -> [Int]
rowColBlock puzzle (rowIndex, colIndex) =
  sort . nub . catMaybes . concat . map elems $ [r, c, b]
    where
      r = row puzzle rowIndex
      c = col puzzle colIndex
      b = block puzzle (rowIndex `div` 3, colIndex `div` 3)

-- Checks whether a row, column, or block is complete.
isComplete :: Array Int Space -> Bool
isComplete = (== map Just [1..9]) . sort . elems

-- Checks whether a puzzle is solved.
isSolved :: Puzzle -> Bool
isSolved puzzle = and . map isComplete $ concat [rows, cols, blocks]
  where
    rows = map (row puzzle) [0..8]
    cols = map (col puzzle) [0..8]
    blocks = map (block puzzle) $ range ((0, 0), (2, 2))
