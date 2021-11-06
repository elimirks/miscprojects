import scala.collection.mutable

// https://leetcode.com/problems/maximum-number-of-achievable-transfer-requests/
// TODO: Look into max flow?
object Solution {
  // Return the maximum number of achievable requests.
  // Main algo idea: the only acceptable transfers _must_ be cycles in the graph
  def maximumRequests(n: Int, requests: Array[Array[Int]]): Int = {
    val graph = createGraph(requests)
    solve(graph)
  }

  def solve(graph: mutable.Map[Int, Node]): Int = {
    val cycles = findGraphCycles(graph)

    cycles.map(cycle => {
      val newGraph = cloneGraph(graph)
      removeCycle(newGraph, cycle)
      cycle.size + solve(newGraph)
    }).maxOption.getOrElse(0)
  }

  def removeCycle(graph: mutable.Map[Int, Node], cycle: List[Int]) {
    edgeReprCycle(cycle).foreach(edge => {
      val (from, to) = edge
      graph.get(from).get.removeEdge(to)
    })
  }

  def findGraphCycles(graph: mutable.Map[Int, Node]): List[List[Int]] = {
    graph.values
      .flatMap(_.findCycles())
      .toList
      .distinctBy(edgeReprCycle)
  }

  def edgeReprCycle(cycle: List[Int]): Set[(Int, Int)] = {
    cycle.zipWithIndex.map({ case (id, index) =>
      val nextIndex = if (index == cycle.size - 1) {
        0
      } else {
        index + 1
      }

      (id, cycle(nextIndex))
    }).toSet
  }

  def createGraph(requests: Array[Array[Int]]): mutable.Map[Int, Node] = {
    val nodes = mutable.Map[Int, Node]()

    requests.foreach({ case Array(from, to) =>
      val fromNode = nodes.getOrElse(from, new Node(from))
      nodes.put(from, fromNode)

      val toNode = nodes.getOrElse(to, new Node(to))
      nodes.put(to, toNode)

      fromNode.addEdge(toNode)
    })

    nodes
  }

  def cloneGraph(graph: mutable.Map[Int, Node]): mutable.Map[Int, Node] = {
    createGraph(
      graph.values
        .flatMap(_.toEdgeArrays())
        .toArray
    )
  }

  class Node(
      id: Int
  ) {
    var edges = List[Node]()

    def getId(): Int =
      id

    def toEdgeArrays(): Array[Array[Int]] =
      edges.map(edge => Array(id, edge.getId())).toArray

    def addEdge(node: Node) {
      edges = node +: edges
    }

    def removeEdge(nodeId: Int) {
      val loc = edges.indexWhere(n => n.getId() == nodeId)
      edges = edges.slice(0, loc) ++ edges.slice(loc + 1, edges.size)
    }

    def findCycles(): List[List[Int]] = {
      edges.flatMap(edge => {
        if (edge.getId() == id) {
          List(List(id))
        } else {
          findCyclesPrime(id, List())
        }
      })
    }

    private def findCyclesPrime(
        startId: Int,
        previous: List[Int]
    ): List[List[Int]] = {
      edges.flatMap(edge => {
        val edgeId = edge.getId()

        if (edgeId == startId) {
          List((edgeId +: previous).reverse)
        } else if (previous.contains(edgeId)) {
          // We'll get this case later, when we call getCycles on this edge node
          List()
        } else {
          edge.findCyclesPrime(startId, edgeId +: previous)
        }
      })
    }
  }
}
