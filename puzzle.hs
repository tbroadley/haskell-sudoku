import Data.Array
import Data.List (sort)

type Space = Maybe Int

type Puzzle = Array (Int, Int) Space

-- Creates a 9x9 sudoku puzzle from a list of spaces. The spaces should be
-- indexed by row and then column, i.e. [(1, 1), (1, 2), ... (2, 1), ...].
makePuzzle :: [Space] -> Puzzle
makePuzzle = listArray ((0, 0), (8, 8))

-- Returns the contents of a given row of a puzzle.
row :: Puzzle -> Int -> Array Int Space
row puzzle rowIndex =
  array (0, 8) [(colIndex, puzzle ! (rowIndex, colIndex)) | colIndex <- [0..8]]

-- Returns the contents of a given column of a puzzle.
col :: Puzzle -> Int -> Array Int Space
col puzzle colIndex =
  array (0, 8) [(rowIndex, puzzle ! (rowIndex, colIndex)) | rowIndex <- [0..8]]

-- Returns the contents of a given block of a puzzle.
block :: Puzzle -> (Int, Int) -> Array Int Space
block puzzle (blockRow, blockCol) =
  listArray (0, 8) [puzzle ! index | index <- range blockBounds]
    where
      blockBounds = ((startRow, startCol), (startRow + 2, startCol + 2))
      startRow = blockRow * 3
      startCol = blockCol * 3

-- Checks whether a row, column, or block is complete.
isComplete :: Array Int Space -> Bool
isComplete = (== map Just [1..9]) . sort . elems
