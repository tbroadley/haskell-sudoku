module Solver (
  Solver
) where

import Puzzle (Puzzle)

-- Class of functions that try to solve sudoku puzzles.
-- If the puzzle cannot be solved, the solver should return Nothing.
type Solver = Puzzle -> Maybe Puzzle
