// https://leetcode.com/explore/challenge/card/august-leetcoding-challenge-2021/615/week-3-august-15th-august-21st/3905/
object Solution {
  type Board = Array[Array[Char]]

  val allValues = Set(
    '1', '2', '3', '4', '5',
    '6', '7', '8', '9', '.'
  )

  def solveSudoku(board: Board): Unit = {
    val solvedBoard = run(board).get

    (0 until 9).zip(0 until 9).map({ case (row, col) =>
      board(row)(col) = solvedBoard(row)(col)
    })
  }

  def run(board: Board): Option[Board] = {
    fill(board)

    val emptyCells = findEmptyCells(board)
    val candidates = emptyCells.map({ case (row, col) =>
      getCellCandidates(board, row, col)
    })

    // Prioritize cells with the least candidates
    val options = emptyCells.zip(candidates).sortBy(_._2.size)

    for (option <- options) {
      val ((row, col), candidates) = option

      for (candidate <- candidates) {
        val newBoard = copyBoard(board)
        newBoard(row)(col) = candidate

        if (isValid(newBoard)) {
          run(newBoard) match {
            case Some(solved) => return Some(solved)
            case None         => ()
          }
        } else {
          None
        }
      }
    }

    None
  }

  def findEmptyCells(board: Board): Seq[(Int, Int)] = {
    val indices = board.flatten.zipWithIndex.filter({ case (c, i) =>
      c == '.'
    }).map(_._2)

    indices.map(index => {
      val row = index / 9
      val col = index % 9
      (row, col)
    })
  }

  def getCellCandidates(board: Board, row: Int, col: Int): Set[Char] = {
    val boxCol = col / 3
    val boxRow = row / 3
    val box = 3 * boxRow + boxCol

    val options = getRow(board, row) ++ getCol(board, col) ++ getBox(board, box)
    options.toSet
  }

  def copyBoard(board: Board): Board = {
    board.map(_.clone())
  }

  def isComplete(board: Board): Boolean = {
    !board.flatten.contains('.')
  }

  def isValid(board: Board): Boolean = {
    (0 until 9).forall(i => groupIsValid(getCol(board, i))) &&
    (0 until 9).forall(i => groupIsValid(getRow(board, i))) &&
    (0 until 9).forall(i => groupIsValid(getBox(board, i)))
  }

  def groupIsValid(group: Seq[Char]): Boolean = {
    val nums = group.filter(_ != '.')
    nums.toSet.size == nums.size
  }

  def getRow(board: Board, rowIndex: Int): Seq[Char] = {
    board(rowIndex)
  }

  def getCol(board: Board, colIndex: Int): Seq[Char] = {
    board.map(row => row(colIndex))
  }

  def getBox(board: Board, boxIndex: Int): Seq[Char] = {
    val rowOffset = (boxIndex * 3) / 9
    val colOffset = (boxIndex * 3) % 9

    (0 until 3).zip(0 until 3).map({ case (row, col) =>
      board(rowOffset + row)(colOffset + col)
    })
  }

  def fill(board: Board) {
    while (
      (0 until 9).map(fillCol(board, _)).foldLeft(false)(_ || _) ||
      (0 until 9).map(fillRow(board, _)).foldLeft(false)(_ || _)
    ) {}
  }

  def fillCol(board: Board, colIndex: Int): Boolean = {
    val col = getCol(board, colIndex)
    val missingSet = allValues.diff(col.toSet)

    missingSet.toSeq match {
      case Seq(el) =>
        val rowIndex = col.indexOf('.')
        board(rowIndex)(colIndex) = el
        true
      case _ => false
    }
  }

  def fillRow(board: Board, rowIndex: Int): Boolean = {
    val row = getRow(board, rowIndex)
    val missingSet = allValues.diff(row.toSet)

    missingSet.toSeq match {
      case Seq(el) =>
        val colIndex = row.indexOf('.')
        board(rowIndex)(colIndex) = el
        true
      case _ => false
    }
  }

  // TODO: ? // def fillBox
}
