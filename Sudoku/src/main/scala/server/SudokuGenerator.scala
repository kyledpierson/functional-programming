package server

import client.SudokuSolver

import scala.util.Random

object SudokuGenerator {
  type Sudoku = Vector[Vector[Int]]

  def initializePuzzle(size: Int, m: Int, n: Int, givens: Int): Sudoku = {
    makePuzzle(size, m, n, givens, size * size / 8)
  }

  def makePuzzle(size: Int, m: Int, n: Int, givens: Int, seeds: Int): Sudoku = {
    val locs = (0 to size - 1).flatMap(i => (0 to size - 1).map(j => (i, j)))
    val emptyPuzzle = Vector.fill(size)(Vector.fill(size)(0))
    val seededPuzzle = plantSeeds(emptyPuzzle, m, n, seeds, Random.shuffle(locs).toVector)
    val fullPuzzle = SudokuSolver.solveSudoku(seededPuzzle, m, n)

    if (fullPuzzle.nonEmpty) {
      val randMap = Random.shuffle(1 to size).toVector
      val mappedPuzzle = fullPuzzle.map(i => i.map(j => randMap(j - 1)))
      val result = pokeHoles(mappedPuzzle, m, n, size * size - givens, Random.shuffle(locs).toVector)

      if (result.isDefined) {
        result.get
      } else {
        println("Couldn't reduce clues to " + givens + " in puzzle of size " + size + " x " + size)
        emptyPuzzle
      }
    } else {
      makePuzzle(size, m, n, givens, seeds / 2)
    }
  }

  def plantSeeds(puzzle: Sudoku, m: Int, n: Int, seeds: Int, locs: Vector[(Int, Int)]): Sudoku = {
    if (seeds == 0 || locs.isEmpty) {
      puzzle
    } else {
      val row = locs.head._1
      val col = locs.head._2
      val numList = Random.shuffle(1 to puzzle.length).toVector
      val result = tryNums(puzzle, m, n, row, col, numList)

      if (result.isDefined) {
        plantSeeds(result.get, m, n, seeds - 1, locs.tail)
      } else {
        plantSeeds(puzzle, m, n, seeds, locs.tail)
      }
    }
  }

  def tryNums(puzzle: Sudoku, m: Int, n: Int, row: Int, col: Int, numList: Vector[Int]): Option[Sudoku] = {
    if (numList.isEmpty) {
      None
    } else if (SudokuSolver.isValid(puzzle, m, n, row, col, numList.head)) {
      Some(puzzle.updated(row, puzzle(row).updated(col, numList.head)))
    } else {
      tryNums(puzzle, m, n, row, col, numList.tail)
    }
  }

  def pokeHoles(puzzle: Sudoku, m: Int, n: Int, holes: Int, locs: Vector[(Int, Int)]): Option[Sudoku] = {
    if (holes == 0) {
      Some(puzzle)
    } else if (locs.isEmpty) {
      None
    } else {
      val row = locs.head._1
      val col = locs.head._2
      val newPuzzle = puzzle.updated(row, puzzle(row).updated(col, 0))

      if (SudokuSolver.solveRest(newPuzzle, m, n, 0, 0, 1, row, col, puzzle(row)(col)).isEmpty) {
        pokeHoles(newPuzzle, m, n, holes - 1, locs.tail)
      } else {
        pokeHoles(puzzle, m, n, holes, locs.tail)
      }
    }
  }

  def toPuzzleString(puzzle: Sudoku): String = {
    puzzle
      .map(i => i.map(j => if (j == 0) "_" else j.toString))
      .map(i => i.mkString(" "))
      .mkString("\n")
  }
}