module Solver (
  CandidatesPuzzle,
  Solver,
  CandidatesSolver,
  toSolver
) where

import Data.Array;

import Puzzle (Space, Puzzle)

-- A type similar to Puzzle, but with a list of candidates for each space.
type CandidatesPuzzle = Array (Int, Int) (Space, [Int])

-- Class of functions that try to solve sudoku puzzles.
-- If the puzzle cannot be solved, the solver should return Nothing.
type Solver = Puzzle -> Maybe Puzzle

-- Class of functions that solve sudoku puzzles using lists of candidates for
-- each square.
-- If the puzzle cannot be solved, the solver should return Nothing.
type CandidatesSolver = CandidatesPuzzle -> Maybe CandidatesPuzzle

-- Adds candidates to a Puzzle, turning it into a CandidatesPuzzle.
-- TODO: write the real addCandidates function.
addCandidates :: Puzzle -> CandidatesPuzzle
addCandidates = fmap (\x -> (x, []))

-- Removes lists of candidates from the spaces of a puzzle.
removeCandidates :: CandidatesPuzzle -> Puzzle
removeCandidates = fmap fst

-- Turns a CandidatesSolver into a Solver by combining it with addCandidates
-- and removeCandidates.
toSolver :: CandidatesSolver -> Solver
toSolver s = (>>= return . removeCandidates) . s . addCandidates
