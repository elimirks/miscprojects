import scala.collection.{mutable => mut}
// https://leetcode.com/problems/surrounded-regions/
object Solution {
  type Board = Array[Array[Char]]

  def solve(board: Board): Unit = {
    val height = board.size
    val width = board(0).size

    // Flip bordered 'O' regions into 'F' regions temporarily
    // The intermediate flipped state 'F' let's us use a lot less memory
    for {
      x <- 0 until width
    } {
      partialFlipBorder(board, x, 0)
      partialFlipBorder(board, x, height - 1)
    }
    for {
      y <- 0 until height
    } {
      partialFlipBorder(board, 0, y)
      partialFlipBorder(board, width - 1, y)
    }

    for {
      y <- 0 until height
      x <- 0 until width
    } {
      if (board(y)(x) == 'O') {
        board(y)(x) = 'X'
      }
    }
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      if (board(y)(x) == 'F') {
        board(y)(x) = 'O'
      }
    }
  }

  def partialFlipBorder(board: Board, x: Int, y: Int): Unit = {
    if (board(y)(x) != 'O') {
      return
    }

    val height = board.size
    val width = board(0).size

    val toSearch = mut.Stack[(Int, Int)]((x, y))

    @inline
    def addToStack(x: Int, y: Int): Unit = {
      if (x < 0 || y < 0 || x == width || y == height) {
      } else if (board(y)(x) == 'O') {
        toSearch.push((x, y))
      }
    }

    while (toSearch.nonEmpty) {
      val (x, y) = toSearch.pop()

      board(y)(x) = 'F'

      addToStack(x - 1, y)
      addToStack(x + 1, y)
      addToStack(x, y - 1)
      addToStack(x, y + 1)
    }
  }
}
