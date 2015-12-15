{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Puzzle (
  Space,
  RowCol,
  GenericPuzzle(GP),
  getPuzzle,
  Puzzle,
  fromFile,
  fromList,
  row,
  col,
  block,
  inBlock,
  rowColBlock,
  isSolved
) where

import Data.Array
import Data.List (sort, nub, intercalate)
import Data.Maybe (catMaybes)
import Data.Char (digitToInt)

type Space = Maybe Int

type RowCol = (Int, Int)

data GenericPuzzle a = GP { getPuzzle :: Array RowCol a }
  deriving (Eq)
type Puzzle = GenericPuzzle Space

instance Show Puzzle where
  show (GP puzzle) =
    intercalate "\n" . map concat . group9 . map toStr . elems $ puzzle
      where
        toStr Nothing = " "
        toStr (Just n) = show n
        group9 [] = []
        group9 l = (take 9 l) : (group9 (drop 9 l))

-- Reads a puzzle from a file.
fromFile :: FilePath -> IO Puzzle
fromFile path = do
  fileContents <- readFile path
  let oneLine = concat $ lines fileContents
  let asMaybes = map toMaybe oneLine
  return . GP $ listArray ((0, 0), (8, 8)) asMaybes
    where
      toMaybe c = if c == '.' then Nothing else Just $ digitToInt c

-- Creates a 9x9 sudoku puzzle from a list of spaces. The spaces should be
-- indexed by row and then column, i.e. [(1, 1), (1, 2), ... (2, 1), ...].
fromList :: [a] -> GenericPuzzle a
fromList = GP . listArray ((0, 0), (8, 8))

-- Returns the contents of a given row of a puzzle.
row :: GenericPuzzle a -> Int -> Array RowCol a
row (GP puzzle) ri =
  array ((ri, 0), (ri, 8)) [((ri, ci), puzzle ! (ri, ci)) | ci <- [0..8]]

-- Returns the contents of a given column of a puzzle.
col :: GenericPuzzle a -> Int -> Array RowCol a
col (GP puzzle) ci =
  array ((0, ci), (8, ci)) [((ri, ci), puzzle ! (ri, ci)) | ri <- [0..8]]

-- Returns the contents of a given block of a puzzle.
block :: GenericPuzzle a -> RowCol -> Array RowCol a
block (GP puzzle) (blockRow, blockCol) =
  listArray blockBounds [puzzle ! index | index <- range blockBounds]
    where
      blockBounds = ((startRow, startCol), (startRow + 2, startCol + 2))
      startRow = blockRow * 3
      startCol = blockCol * 3

-- Returns the block that a given space is in.
inBlock :: RowCol -> RowCol
inBlock (rowIndex, colIndex) = (rowIndex `div` 3, colIndex `div` 3)

-- Get a list of the unique numbers in the row, column, and block that a space
-- is in.
rowColBlock :: Puzzle -> RowCol -> [Int]
rowColBlock puzzle rc@(rowIndex, colIndex) =
  sort . nub . catMaybes . concat . map elems $ [r, c, b]
    where
      r = row puzzle rowIndex
      c = col puzzle colIndex
      b = block puzzle (inBlock rc)

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
