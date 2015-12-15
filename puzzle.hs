import Data.Array

type Space = Maybe Int

type Puzzle = Array (Int, Int) Space

-- Creates a 9x9 sudoku puzzle from a list of spaces. The spaces should be
-- indexed by row and then column, i.e. [(1, 1), (1, 2), ... (2, 1), ...].
makePuzzle :: [Space] -> Puzzle
makePuzzle = listArray ((1, 1), (9, 9))

-- Returns the contents of a given row of a puzzle.
row :: Int -> Puzzle -> Array Int Space
row rowIndex puzzle =
  array (1, 9) [(colIndex, puzzle ! (rowIndex, colIndex)) | colIndex <- [1..9]]

-- Returns the contents of a given column of a puzzle.
col :: Int -> Puzzle -> Array Int Space
col colIndex puzzle =
  array (1, 9) [(rowIndex, puzzle ! (rowIndex, colIndex)) | rowIndex <- [1..9]]
