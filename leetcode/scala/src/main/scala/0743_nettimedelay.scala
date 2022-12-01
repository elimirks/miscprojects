import scala.collection.{mutable => mut}
// https://leetcode.com/problems/network-delay-time/
/*
 * The obvious solution is to construct a graph and traverse it...
 * however that isn't ideal for runtime complexity. And it requires extra space
 *
 * Dijkstra maybe?
 */
object Solution {
  // times stores Array(sourceNode, targetNode, travelTime)
  def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int = {
    val graph = createGraph(times, n)
    maxDelayTime(graph, k)
  }

  def maxDelayTime(graph: Map[Int, Node], startId: Int): Int = {
    graph.get(startId).get.propagate(0)
    val maxTime = graph.values.map(_.travelTime).max
    if (maxTime == Int.MaxValue) -1 else maxTime
  }

  // O(n) runtime
  def createGraph(times: Array[Array[Int]], nodeCount: Int): Map[Int, Node] = {
    val map = mut.HashMap[Int, Node]()
    for (i <- 1 to nodeCount) {
      map.put(i, new Node())
    }
    for (Array(sourceId, targetId, travelTime) <- times) {
      val sourceNode = map.get(sourceId).get
      val targetNode = map.get(targetId).get
      sourceNode.edges.addOne((travelTime, targetNode))
      map.put(sourceId, sourceNode)
      map.put(targetId, targetNode)
    }
    map.toMap
  }
}

class Node() {
  // (edgeTime, nodeObject)
  val edges = mut.ArrayBuffer[(Int, Node)]()
  var travelTime: Int = Int.MaxValue

  // Propagate travel time throughout the node
  // Tries to minimize travel time
  def propagate(newTime: Int): Unit = {
    if (newTime < travelTime) {
      travelTime = newTime

      for ((edgeTime, edgeNode) <- edges) {
        edgeNode.propagate(edgeTime + travelTime)
      }
    }
  }
}
