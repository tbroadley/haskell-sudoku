module SimpleSolver (
  solve
) where

import Solver (Solver)

-- Solves a sudoku puzzle in a simple way. For each space, it calculates the
-- possible numbers that can be placed in the space. If no space has exactly
-- one number that can be placed there, the puzzle cannot be solved. Otherwise,
-- it places the one possible number in the space, and updates the possible
-- numbers for each cell in its row, column, and block.
solve :: Solver
solve = Just
