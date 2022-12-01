import scala.collection.mutable

// https://leetcode.com/problems/search-suggestions-system/
object Solution {
  def suggestedProducts(
    products: Array[String],
    searchWord: String
  ): List[List[String]] = {
    var node = new Trie()
    for (product <- products) {
      node.add(product)
    }
    val result = mutable.ListBuffer[List[String]]()
    for (c <- searchWord.toSeq) {
      node = node.children.getOrElse(c, new Trie())
      result.append(node.getChildren(3))
    }
    result.toList
  }
}

class Trie {
  var value: Option[String] = None
  val children = mutable.Map[Char, Trie]()

  // Adds a word to this trie
  def add(word: String): Unit = {
    addWord(word.toList, word)
  }

  // Attempts to get `count` children in lexicographical order
  def getChildren(count: Int): List[String] = {
    if (count <= 0) {
      return List()
    }
    var result = value match {
      case Some(value) => List(value)
      case _           => List()
    }
    val sortedKeys = children.keys.toSeq.sorted
    for (key <- sortedKeys) {
      val subTrie = children.get(key).get
      result = result ++ subTrie.getChildren(count - result.size)
    }
    result
  }

  private def addWord(xs: List[Char], word: String): Unit = {
    xs match {
      case Nil => this.value = Some(word)
      case x::xs => children.get(x) match {
        case Some(subTrie) =>
          subTrie.addWord(xs, word)
        case None =>
          val subTrie = new Trie()
          subTrie.addWord(xs, word)
          children.put(x, subTrie)
      }
    }
  }
}
