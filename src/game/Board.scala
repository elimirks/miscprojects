package game

import java.awt.Color
import java.awt.Graphics
import javax.swing.JPanel

/**
 * @constructor Create a new boards with the given tiles (unsorted is OK).
 * @param tiles The tiles to create for the board.
 */
class Board(tiles: List[Tile]) extends JPanel {
  setBackground(Color.BLACK)

  // Sort the tiles so that when they render, they layer properly.
  private val _tiles = tiles.sortWith((a, b) => a.z < b.z || a.x < b.x)

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)

    for (tile <- _tiles) {
      val originX = 128
      val originY = 128

      val x = tile.x*tile.size/2 - tile.y*tile.size/2
      val y = tile.x*tile.size/4 + tile.y*tile.size/4 - tile.z*tile.size/2

      g.drawImage(tile.image, x, y, tile.size, tile.size, null)
    }
  }
}

