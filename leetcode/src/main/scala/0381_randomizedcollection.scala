import scala.collection.mutable
import scala.util.Random

/**
  * https://leetcode.com/problems/insert-delete-getrandom-o1-duplicates-allowed/
  * Your RandomizedCollection object will be instantiated and called as such:
  * var obj = new RandomizedCollection()
  * var param_1 = obj.insert(`val`)
  * var param_2 = obj.remove(`val`)
  * var param_3 = obj.getRandom()
  */
class RandomizedCollection() {
  val valueToIndexes = mutable.Map[Int, mutable.Set[Int]]()
  val indexToValue = mutable.ArrayBuffer[Int]()

  def insert(value: Int): Boolean = {
    val indexes = valueToIndexes.getOrElse(value, mutable.Set[Int]())
    val notPresent = indexes.isEmpty

    indexes.add(indexToValue.size)
    valueToIndexes.put(value, indexes)

    indexToValue.append(value)

    notPresent
  }

  def remove(value: Int): Boolean = {
    val indexes = valueToIndexes.getOrElse(value, mutable.Set[Int]())

    if (indexes.isEmpty) {
      // Do nothing! It's alerady gone
      false
    } else {
      val indexToRemove = indexes.head
      indexes.remove(indexToRemove)

      val maxIndex = indexToValue.size - 1
      // "shift back" the max index to the spot we removed
      if (maxIndex != indexToRemove) {
        indexToValue(indexToRemove) = indexToValue(maxIndex)

        val oldMaxIndexes = valueToIndexes.get(indexToValue(maxIndex)).get
        oldMaxIndexes.remove(maxIndex)
        oldMaxIndexes.add(indexToRemove)
      }

      indexToValue.trimEnd(1)

      // Clean up old junk if necessary
      if (indexes.isEmpty) {
        valueToIndexes.remove(value)
      }
      true
    }
  }

  def getRandom(): Int = {
    indexToValue(Random.nextInt(indexToValue.size))
  }
}
