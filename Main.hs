import Puzzle (Puzzle, fromFile)
import SinglesSolver (solve)

testFile :: IO Puzzle
testFile = fromFile "test.txt"

solvedTestFile :: IO Puzzle
solvedTestFile = testFile >>= return . solve
