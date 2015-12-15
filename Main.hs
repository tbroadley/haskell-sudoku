import Puzzle (Puzzle, fromFile, isSolved)
import SinglesSolver (solve)

testFile :: IO Puzzle
testFile = fromFile "test.txt"

solvedTestFile :: IO Puzzle
solvedTestFile = testFile >>= return . solve

isSolvedTestFile :: IO Bool
isSolvedTestFile = solvedTestFile >>= return . isSolved
