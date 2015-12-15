module Solver (
  solve
) where

import Puzzle (Space, Puzzle)

-- Class of functions that try to solve sudoku puzzles.
-- If the puzzle cannot be solved, the solver should return Nothing.
type Solver = Puzzle -> Maybe Puzzle
