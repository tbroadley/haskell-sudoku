module SinglesSolver (
  solve
) where

import Solver (CandidatesPuzzle, Solver, CandidatesSolver, toSolver, updatePuzzle)

import Data.Array

-- Solves a sudoku puzzle in a simple way. For each space, it calculates a list
-- of candidates. If no space has exactly one candidate, the puzzle cannot be
-- solved. Otherwise, it places the candidate in the space, and updates the
-- candidates for each cell in its row, column, and block.
solve :: Solver
solve = toSolver solveOneStep

-- Progresses one step in the solution algorithm.
solveOneStep :: CandidatesSolver
solveOneStep puzzle = case oneCandidate of
  []                        -> puzzle
  ((rc, (Nothing, _)):_)    -> puzzle
  ((rc, (Just value, _)):_) -> updatePuzzle rc value puzzle 
  where
    oneCandidate = filter hasOneCandidate $ assocs puzzle
    hasOneCandidate (_, (_, candidates)) = length candidates == 1
