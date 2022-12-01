import scala.collection.mutable
import scala.util.Random

/**
 * https://leetcode.com/problems/insert-delete-getrandom-o1/
 * Your RandomizedSet object will be instantiated and called as such:
 * var obj = new RandomizedSet()
 * var param_1 = obj.insert(`val`)
 * var param_2 = obj.remove(`val`)
 * var param_3 = obj.getRandom()
 */
class RandomizedSet {
  val valueToIndex = mutable.Map[Int, Int]()
  val indexToValue = mutable.ArrayBuffer[Int]()

  def insert(value: Int): Boolean = {
    val index = valueToIndex.getOrElse(value, -1)

    if (index > -1) {
      // Do nothing, it's already here!
      false
    } else {
      val newIndex = indexToValue.size
      indexToValue.append(value)
      valueToIndex.put(value, newIndex)
      true
    }
  }

  def remove(value: Int): Boolean = {
    val index = valueToIndex.getOrElse(value, -1)

    if (index > -1) {
      val maxIndex = indexToValue.size - 1
      // "shift back" the max index to the spot we removed
      if (maxIndex != index) {
        indexToValue(index) = indexToValue(maxIndex)
        valueToIndex.put(indexToValue(index), index)
      }
      indexToValue.trimEnd(1)
      valueToIndex.remove(value)
      true
    } else {
      // Do nothing! It's alerady gone
      false
    }
  }

  def getRandom(): Int = {
    indexToValue(Random.nextInt(indexToValue.size))
  }
}
