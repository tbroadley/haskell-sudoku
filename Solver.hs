{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Solver (
  CandidatesPuzzle,
  Solver,
  CandidatesSolver,
  toSolver,
  updatePuzzle
) where

import Data.Array
import Data.Maybe (isJust)
import Data.List (nub, intercalate)

import Puzzle (Space, RowCol, GenericPuzzle(GP), getPuzzle, Puzzle, fromList, row, col, block, inBlock, rowColBlock)

type Candidate = Int

-- A type similar to Puzzle, but with a list of candidates for each space.
type CandidatesPuzzle = GenericPuzzle (Space, [Candidate])

instance Show CandidatesPuzzle where
  show (GP puzzle) =
    intercalate "\n" . map (intercalate ",") . group9 . map toStr . elems $ puzzle
      where
        toStr (Nothing, l) = show l
        toStr (Just n, _) = show n
        group9 [] = []
        group9 l = (take 9 l) : (group9 (drop 9 l))

-- Class of functions that try to solve sudoku puzzles.
-- If the puzzle cannot be solved, the solver should return the last state the
-- solver was able to solve to.
type Solver = Puzzle -> Puzzle

-- Class of functions that solve sudoku puzzles using lists of candidates for
-- each square.
-- If the puzzle cannot be solved, the solver should return the same state it
-- was passed.
type CandidatesSolver = CandidatesPuzzle -> CandidatesPuzzle

-- Adds candidates to a Puzzle, turning it into a CandidatesPuzzle.
addCandidates :: Puzzle -> CandidatesPuzzle
addCandidates p@(GP puzzle) = fromList $ zipWith (,) spaces candidates
  where
    spaces = elems puzzle
    candidates = map getCandidates $ range ((0, 0), (8, 8))
    getCandidates rc = case puzzle ! rc of
      Just _  -> []
      Nothing -> filter (\n -> notElem n $ rowColBlock p rc) [1..9]

-- Removes lists of candidates from the spaces of a puzzle.
removeCandidates :: CandidatesPuzzle -> Puzzle
removeCandidates = GP . fmap fst . getPuzzle

-- Turns a CandidatesSolver into a Solver by combining it with addCandidates
-- and removeCandidates.
toSolver :: CandidatesSolver -> Solver
toSolver solve = removeCandidates . solveWithCandidates . addCandidates
  where
    solveWithCandidates puzzle
      | isComplete puzzle || puzzle == nextPuzzle = puzzle
      | otherwise                                 = solveWithCandidates nextPuzzle
        where
          nextPuzzle = solve puzzle

-- Checks whether a CandidatesPuzzle is complete.
isComplete :: CandidatesPuzzle -> Bool
isComplete = and . fmap (isJust . fst) . getPuzzle

-- Updates a puzzle with the given value in the given row and column, and
-- updates the candidates of all cells in the same row, column, and block.
updatePuzzle :: RowCol -> Candidate -> CandidatesPuzzle -> CandidatesPuzzle
updatePuzzle rc@(rowIndex, colIndex) value p@(GP puzzle) =
  GP $ puzzle // updateRowColBlock // [(rc, (Just value, []))]
    where
      updateRowColBlock = map filterValue . nub . concat $ map assocs [r, c, b]
      filterValue (k, (v, candidates)) = (k, (v, filter (/= value) candidates))
      r = row p rowIndex
      c = col p colIndex
      b = block p (inBlock rc)
