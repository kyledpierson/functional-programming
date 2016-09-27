/**
  * Name:       Kyle Pierson
  * Project:    HW1 - Sudoku Solver
  * Class:      CS 5965 - Functional Programming Studio
  * Instructor: Prof. Matthew Flatt
  */

package client

object SudokuSolver {

  type Sudoku = Vector[Vector[Int]]

  def solveSudoku(puzzle: Sudoku, m: Int, n: Int): Sudoku = {
    val result = solveRest(puzzle, m, n, 0, 0, 1, 0, 0, 0)

    if (result.isDefined) {
      result.get
    } else {
      Vector[Vector[Int]]()
    }
  }

  def solveRest(puzzle: Sudoku, m: Int, n: Int,
                row: Int, col: Int, num: Int,
                illRow: Int, illCol: Int, illNum: Int): Option[Sudoku] = {
    if (num > puzzle.length) {
      None
    } else if (row == puzzle.length) {
      Some(puzzle)
    } else if (col == puzzle(row).length) {
      solveRest(puzzle, m, n, row + 1, 0, 1, illRow, illCol, illNum)
    } else if (puzzle(row)(col) != 0) {
      solveRest(puzzle, m, n, row, col + 1, 1, illRow, illCol, illNum)
    } else if (!(row == illRow && col == illCol && num == illNum) && isValid(puzzle, m, n, row, col, num)) {
      val newPuzzle = puzzle.updated(row, puzzle(row).updated(col, num))
      val result = solveRest(newPuzzle, m, n, row, col + 1, 1, illRow, illCol, illNum)

      if (result.isDefined) {
        result
      } else {
        solveRest(puzzle, m, n, row, col, num + 1, illRow, illCol, illNum)
      }
    } else {
      solveRest(puzzle, m, n, row, col, num + 1, illRow, illCol, illNum)
    }
  }

  def isValid(puzzle: Sudoku, m: Int, n: Int, row: Int, col: Int, num: Int): Boolean = {
    val startRow = row - row % n
    val startCol = col - col % m
    val endRow = startRow + n - 1
    val endCol = startCol + m - 1

    !(inRange(puzzle, row, row, 0, 0, puzzle(row).length - 1, num) // In the current row?
      || inRange(puzzle, 0, puzzle.length - 1, col, col, col, num) // In the current column?
      || inRange(puzzle, startRow, endRow, startCol, startCol, endCol, num)) // In the current box?
  }

  def inRange(puzzle: Sudoku, curRow: Int, endRow: Int,
              startCol: Int, curCol: Int, endCol: Int, target: Int): Boolean = {
    if (puzzle(curRow)(curCol) == target) {
      true
    } else if (curCol < endCol) {
      inRange(puzzle, curRow, endRow, startCol, curCol + 1, endCol, target)
    } else if (curRow < endRow) {
      inRange(puzzle, curRow + 1, endRow, startCol, startCol, endCol, target)
    } else {
      false
    }
  }
}