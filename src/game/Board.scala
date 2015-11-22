package game

import java.awt.Color
import java.awt.Graphics
import javax.swing.JPanel

class Board extends JPanel {
  setBackground(Color.BLACK)

  // Make a cozy little map
  private val tiles = Array(
    new Tile(4, 1, 0, spriteIndex = 23),
    new Tile(4, 2, 0, spriteIndex = 3),
    new Tile(5, 1, 0, spriteIndex = 28),
    new Tile(5, 2, 0, spriteIndex = 31),
    new Tile(4, 2, 1, spriteIndex = 0),
    new Tile(5, 1, 1, spriteIndex = 48),
    new Tile(6, 1, 0, spriteIndex = 0),
    new Tile(6, 2, 0, spriteIndex = 0),
    new Tile(6, 1, 1, spriteIndex = 44),
    new Tile(6, 2, 1, spriteIndex = 54)
  )

  override def paintComponent(g: Graphics) {
    super.paintComponent(g)

    for (tile <- tiles) {
      val originX = 128
      val originY = 128

      val x = tile.x*tile.size/2 - tile.y*tile.size/2
      val y = tile.x*tile.size/4 + tile.y*tile.size/4 - tile.z*tile.size/2

      g.drawImage(tile.image, x, y, tile.size, tile.size, null)
    }
  }
}

