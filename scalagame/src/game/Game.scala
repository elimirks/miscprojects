package game

import javax.swing.JFrame

class Game extends JFrame {
  // Make a cozy little map.
  // Eventually, we should parse this data from a file.
  private val tiles = List(
    new Tile(5, 1, 1, spriteIndex = 48),
    new Tile(4, 2, 0, spriteIndex = 3),
    new Tile(5, 1, 0, spriteIndex = 28),
    new Tile(5, 2, 0, spriteIndex = 31),
    new Tile(4, 2, 1, spriteIndex = 0),
    new Tile(6, 1, 0, spriteIndex = 0),
    new Tile(6, 2, 0, spriteIndex = 0),
    new Tile(4, 1, 0, spriteIndex = 23),
    new Tile(6, 1, 1, spriteIndex = 44),
    new Tile(6, 2, 1, spriteIndex = 54)
  )

	add(new Board(tiles))

	setSize(512, 512)
	setTitle("All the Scala!!!")
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	setLocationRelativeTo(null)
}

