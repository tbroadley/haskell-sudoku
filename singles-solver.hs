module SinglesSolver (
  solve
) where

import Solver (CandidatesPuzzle, Solver, CandidatesSolver, toSolver, updatePuzzle)

import Data.Array
import Data.Maybe (isJust)

-- Solves a sudoku puzzle in a simple way. For each space, it calculates a list
-- of candidates. If no space has exactly one candidate, the puzzle cannot be
-- solved. Otherwise, it places the candidate in the space, and updates the
-- candidates for each cell in its row, column, and block.
solve :: Solver
solve = toSolver solveWithCandidates

solveWithCandidates :: CandidatesSolver
solveWithCandidates puzzle
  | isComplete puzzle || puzzle == nextPuzzle = puzzle
  | otherwise                                 = solveWithCandidates nextPuzzle
    where
      nextPuzzle = solveOneStep puzzle

-- Progresses one step in the solution algorithm.
solveOneStep :: CandidatesSolver
solveOneStep puzzle = case oneCandidate of
  []                        -> puzzle
  ((rc, (Nothing, _)):_)    -> puzzle
  ((rc, (Just value, _)):_) -> updatePuzzle puzzle rc value
  where
    oneCandidate = filter hasOneCandidate $ assocs puzzle
    hasOneCandidate (_, (_, candidates)) = length candidates == 1

-- Checks whether a CandidatesPuzzle is complete.
isComplete :: CandidatesPuzzle -> Bool
isComplete = and . fmap (isJust . fst)
