module Solver (
  CandidatesPuzzle,
  Solver,
  CandidatesSolver,
  toSolver
) where

import Data.Array

import Puzzle (Space, GenericPuzzle, Puzzle, fromList, rowColBlock)

-- A type similar to Puzzle, but with a list of candidates for each space.
type CandidatesPuzzle = GenericPuzzle (Space, [Int])

-- Class of functions that try to solve sudoku puzzles.
-- If the puzzle cannot be solved, the solver should return Nothing.
type Solver = Puzzle -> Maybe (Puzzle)

-- Class of functions that solve sudoku puzzles using lists of candidates for
-- each square.
-- If the puzzle cannot be solved, the solver should return Nothing.
type CandidatesSolver = CandidatesPuzzle -> Maybe CandidatesPuzzle

-- Adds candidates to a Puzzle, turning it into a CandidatesPuzzle.
addCandidates :: Puzzle -> CandidatesPuzzle
addCandidates puzzle = fromList $ zipWith (,) spaces candidates
  where
    spaces = elems puzzle
    candidates = map getCandidates $ range ((0, 0), (8, 8))
    getCandidates rc = filter (\n -> notElem n $ rowColBlock puzzle rc) [1..9]

-- Removes lists of candidates from the spaces of a puzzle.
removeCandidates :: CandidatesPuzzle -> Puzzle
removeCandidates = fmap fst

-- Turns a CandidatesSolver into a Solver by combining it with addCandidates
-- and removeCandidates.
toSolver :: CandidatesSolver -> Solver
toSolver s = (>>= return . removeCandidates) . s . addCandidates
