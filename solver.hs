module Solver (
  solve
) where

import Puzzle (Space, Puzzle)

-- Solves a sudoku puzzle. If the puzzle cannot be solved, returns Nothing.
solve :: Puzzle -> Maybe Puzzle
solve = Just
