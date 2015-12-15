import Data.Array

type Space = Maybe Int

type Puzzle = Array (Int, Int) Space

-- Creates a 9x9 sudoku puzzle from a list of spaces. The spaces should be
-- indexed by row and then column, i.e. [(1, 1), (1, 2), ... (2, 1), ...].
makePuzzle :: [Space] -> Puzzle
makePuzzle = listArray ((0, 0), (8, 8))

-- Returns the contents of a given row of a puzzle.
row :: Int -> Puzzle -> Array Int Space
row rowIndex puzzle =
  array (0, 8) [(colIndex, puzzle ! (rowIndex, colIndex)) | colIndex <- [0..8]]

-- Returns the contents of a given column of a puzzle.
col :: Int -> Puzzle -> Array Int Space
col colIndex puzzle =
  array (0, 8) [(rowIndex, puzzle ! (rowIndex, colIndex)) | rowIndex <- [0..8]]

-- Returns the contents of a given block of a puzzle.
block :: (Int, Int) -> Puzzle -> Array Int Space
block (blockRow, blockCol) puzzle =
  listArray (0, 8) [puzzle ! index | index <- range blockBounds]
    where
      blockBounds = ((startRow, startCol), (startRow + 2, startCol + 2))
      startRow = blockRow * 3
      startCol = blockCol * 3
