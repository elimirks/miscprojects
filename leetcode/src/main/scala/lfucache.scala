import scala.collection.mutable

/**
  * Your LFUCache object will be instantiated and called as such:
  * var obj = new LFUCache(capacity)
  * var param_1 = obj.get(key)
  * obj.put(key,value)
  * https://leetcode.com/problems/lfu-cache/
  */
class LFUCache(_capacity: Int) {
  val entries = mutable.Map[Int, Int]()

  var transactionId: Long = 0
  val history = mutable.Map[Int, (Int, Long)]()

  /**
    * Gets the value of the key if the key exists in the cache.
    * Otherwise, returns -1.
    * Must be _average_ O(1)
    */
  def get(key: Int): Int = {
    if (entries.contains(key)) {
      incrementUse(key)
    }
    entries.getOrElse(key, -1)
  }

  /**
    * Update the value of the key if present, or inserts the key if not already present.
    * When the cache reaches its capacity, it should invalidate and remove the
    * least frequently used key before inserting a new item.
    * For this problem, when there is a
    * tie (i.e., two or more keys with the same frequency), the
    * LEAST RECENTLY used key would be invalidated.
    * Must be _average_ O(1)
    */
  def put(key: Int, value: Int) {
    entries.put(key, value)
    incrementUse(key)

    if (entries.size > _capacity) {
      evict()
    }
  }

  // O(1)
  def incrementUse(key: Int) {
    val (useCount, _) = history.getOrElse(key, (0, 0))
    history.put(key, (useCount + 1, transactionId))
    transactionId += 1
  }

  // FIXME: Rubbish O(n) performance
  def evict() {
    if (history.size == 0) return;

    var (minKey, (minCount, minId)) = history.head

    history.keys.foreach(key => {
      val (count, id) = history.get(key).get

      if (count < minCount || (count == minCount && id < minId)) {
        minKey   = key
        minCount = count
        minId    = id
      }
    })

    history.remove(minKey)
    entries.remove(minKey)
  }
}
