package game

import java.io.File
import java.io.IOException
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object Tile {
  private val spriteFile = new File("basic_ground_tiles.png")
  private val _image =
    try
      ImageIO.read(spriteFile)
    catch {
      case e: IOException =>
        throw new RuntimeException("Could not find sprite file!", e)
    }
}

class Tile(X: Int, Y: Int, Z: Int, spriteIndex: Int = 0) {
  private val _x = X
  private val _y = Y
  private val _z = Z
  private val _spriteIndex = spriteIndex

  def image = Tile._image.getSubimage(
    128 * (spriteIndex%8),
    128 * (spriteIndex/8),
    128, 128)
  def size = 64
  def x = _x
  def y = _y
  def z = _z
}

