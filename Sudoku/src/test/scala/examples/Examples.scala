/**
  * Name:       Kyle Pierson
  * Project:    HW1 - Sudoku Solver
  * Class:      CS 5965 - Functional Programming Studio
  * Instructor: Prof. Matthew Flatt
  */

package examples

import client.SudokuSolver
import server.SudokuGenerator

object Examples extends App {

  //TEST SOLVER
  val puzzle1 = Vector(
    Vector(0, 0, 2, 0, 0, 5, 0, 7, 9),
    Vector(1, 0, 5, 0, 0, 3, 0, 0, 0),
    Vector(0, 0, 0, 0, 0, 0, 6, 0, 0),
    Vector(0, 1, 0, 4, 0, 0, 9, 0, 0),
    Vector(0, 9, 0, 0, 0, 0, 0, 8, 0),
    Vector(0, 0, 4, 0, 0, 9, 0, 1, 0),
    Vector(0, 0, 9, 0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 1, 0, 0, 3, 0, 6),
    Vector(6, 8, 0, 3, 0, 0, 4, 0, 0)
  )
  val answer1 = Vector(
    Vector(3, 6, 2, 8, 4, 5, 1, 7, 9),
    Vector(1, 7, 5, 9, 6, 3, 2, 4, 8),
    Vector(9, 4, 8, 2, 1, 7, 6, 3, 5),
    Vector(7, 1, 3, 4, 5, 8, 9, 6, 2),
    Vector(2, 9, 6, 7, 3, 1, 5, 8, 4),
    Vector(8, 5, 4, 6, 2, 9, 7, 1, 3),
    Vector(4, 3, 9, 5, 7, 6, 8, 2, 1),
    Vector(5, 2, 7, 1, 8, 4, 3, 9, 6),
    Vector(6, 8, 1, 3, 9, 2, 4, 5, 7)
  )
  val result1 = SudokuSolver.solveSudoku(puzzle1, 3, 3)
  assert(result1 equals answer1)

  // TEST GENERATOR
  val randomPuzzle1 = SudokuGenerator.initializePuzzle(6, 2, 3, 15)
  val solvedRandom1 = SudokuSolver.solveSudoku(randomPuzzle1, 2, 3)

  for (idx1 <- randomPuzzle1.indices) {
    for (idx2 <- randomPuzzle1(idx1).indices) {
      if (randomPuzzle1(idx1)(idx2) == 0)
        assert(SudokuSolver.solveRest(randomPuzzle1, 2, 3, 0, 0, 1, idx1, idx2, solvedRandom1(idx1)(idx2)).isEmpty)
    }
  }

  val randomPuzzle2 = SudokuGenerator.initializePuzzle(9, 3, 3, 30)
  val solvedRandom2 = SudokuSolver.solveSudoku(randomPuzzle2, 3, 3)

  for (idx1 <- randomPuzzle2.indices) {
    for (idx2 <- randomPuzzle2(idx1).indices) {
      if (randomPuzzle2(idx1)(idx2) == 0)
        assert(SudokuSolver.solveRest(randomPuzzle2, 3, 3, 0, 0, 1, idx1, idx2, solvedRandom2(idx1)(idx2)).isEmpty)
    }
  }

  val randomPuzzle3 = SudokuGenerator.initializePuzzle(12, 4, 3, 60)
  val solvedRandom3 = SudokuSolver.solveSudoku(randomPuzzle3, 4, 3)

  for (idx1 <- randomPuzzle3.indices) {
    for (idx2 <- randomPuzzle3(idx1).indices) {
      if (randomPuzzle3(idx1)(idx2) == 0)
        assert(SudokuSolver.solveRest(randomPuzzle3, 4, 3, 0, 0, 1, idx1, idx2, solvedRandom3(idx1)(idx2)).isEmpty)
    }
  }

  // ----------------------------------------------------------------------
  // SUDOKU PRINTING
  // NOT FUNCTIONAL - IGNORE THIS CODE (UNCOMMENT TO SEE PRINTED PUZZLES)
  // ----------------------------------------------------------------------
  //  def printPuzzle(puzzle: Sudoku, m: Int, n: Int) = {
  //    val max = m * n
  //    val divNum = (puzzle(0).length + (puzzle(0).length / m)) * 2
  //    var div = ""
  //    for (num <- 0 to divNum)
  //      div = div + "-"
  //
  //    if (max >= 10)
  //      for (num <- puzzle(0).indices)
  //        div = div + "-"
  //
  //    for (i <- puzzle.indices) {
  //      if (i % n == 0) println(div)
  //      for (j <- puzzle(i).indices) {
  //        if (j % m == 0) print("| ")
  //        print(pad(puzzle(i)(j), max) + " ")
  //      }
  //      print("|\n")
  //    }
  //    println(div)
  //  }
  //
  //  def pad(num: Int, max: Int): String = {
  //    if (num < 10 && max >= 10) {
  //      "0" + num
  //    } else {
  //      "" + num
  //    }
  //  }
  //
  //  println("Puzzle 1")
  //  printPuzzle(puzzle1, 3, 3)
  //  println("Puzzle 1 Solution")
  //  printPuzzle(result1, 3, 3)
  //
  //  println("Random Puzzle 1")
  //  printPuzzle(randomPuzzle1, 2, 3)
  //  println("Random Puzzle 1 Solution")
  //  printPuzzle(solvedRandom1, 2, 3)
  //
  //  println("Random Puzzle 2")
  //  printPuzzle(randomPuzzle2, 3, 3)
  //  println("Random Puzzle 2 Solution")
  //  printPuzzle(solvedRandom2, 3, 3)
  //
  //  println("Random Puzzle 3")
  //  printPuzzle(randomPuzzle3, 4, 3)
  //  println("Random Puzzle 3 Solution")
  //  printPuzzle(solvedRandom3, 4, 3)
}