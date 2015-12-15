module SinglesSolver (
  solve
) where

import Solver (Solver)

-- Solves a sudoku puzzle in a simple way. For each space, it calculates a list
-- of candidates. If no space has exactly one candidate, the puzzle cannot be
-- solved. Otherwise, it places the candidate in the space, and updates the
-- candidates for each cell in its row, column, and block.
solve :: Solver
solve = Just
