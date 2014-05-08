package edu.gatech.eilab.scheherazade.graph

import edu.gatech.eilab.scheherazade.data._
/**
 * Contains a number of sample graphs. Graph algorithms can be easily tested on them.
 *
 */
object SampleGraph {

  /**
   *      1
   *      /  \
   *     2    3
   *         / \
   *        4   5
   *         \  /
   *           6
   *          /
   *         7
   */
  val sample1: Graph = {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)

    val links = List(
      new Link(c1, c2),
      new Link(c1, c3),
      new Link(c3, c4),
      new Link(c3, c5),
      new Link(c4, c6),
      new Link(c5, c6),
      new Link(c6, c7))

    new Graph(List(c1, c2, c3, c4, c5, c6, c7), links)
  }

  /**
   *      2        5
   * 1 -> { 3 } -> { 6 } -> 8
   *        4        7
   *
   * Each node in {2, 3, 4} is connected to 2 nodes in {5, 6, 7}
   */
  val sample2: Graph = {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)
    val c8 = new Cluster("C8", Nil)

    val links = List(
      new Link(c1, c2),
      new Link(c1, c3),
      new Link(c1, c4),
      new Link(c2, c5),
      new Link(c2, c6),
      new Link(c3, c6),
      new Link(c3, c7),
      new Link(c4, c7),
      new Link(c4, c5),
      new Link(c5, c8),
      new Link(c6, c8),
      new Link(c7, c8))

    new Graph(List(c1, c2, c3, c4, c5, c6, c7, c8), links)
  }

  /**
   * DAG from left to right. This graph could create difficulties in computing levels
   *     C3--C4--C5--C9
   *    /         \
   *  C1           \   C8
   *    \           \ /
   *     C2---------C6--C7
   */
  val sample3: Graph = {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)
    val c8 = new Cluster("C8", Nil)
    val c9 = new Cluster("C9", Nil)

    val links = List(
      new Link(c1, c2),
      new Link(c1, c3),
      new Link(c3, c4),
      new Link(c4, c5),
      new Link(c5, c6),
      new Link(c2, c6),
      new Link(c6, c7),
      new Link(c6, c8),
      new Link(c5, c9))

    new Graph(List(c1, c2, c3, c4, c5, c6, c7, c8, c9), links)
  }

  /**
   * DAG from left to right. This one has mutual exclusion relations
   *     C3--C4--C5--C9
   *    /  \    /
   *  C1    C10     C8
   *    \          /
   *     C2--C6--C7
   */
  val sample4: Graph = {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)
    val c5 = new Cluster("C5", Nil)
    val c6 = new Cluster("C6", Nil)
    val c7 = new Cluster("C7", Nil)
    val c8 = new Cluster("C8", Nil)
    val c9 = new Cluster("C9", Nil)
    val c10 = new Cluster("C10", Nil)

    val nodes = List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)

    val links = List(
      new Link(c1, c2),
      new Link(c1, c3),
      new Link(c3, c4),
      new Link(c3, c10),
      new Link(c4, c5),
      new Link(c10, c5),
      new Link(c5, c9),
      new Link(c2, c6),
      new Link(c6, c7),
      new Link(c7, c8))

    val me = List(
      new MutualExcl(c2, c3),
      new MutualExcl(c4, c10))

    new Graph(nodes, links, me)
  }

  def sample5(): Graph =
    {
      val c1 = new Cluster("C1", Nil)
      val c2 = new Cluster("C2", Nil)
      val c3 = new Cluster("C3", Nil)
      val c4 = new Cluster("C4", Nil)
      val c5 = new Cluster("C5", Nil)
      val c6 = new Cluster("C6", Nil)
      val c7 = new Cluster("C7", Nil)
      val c8 = new Cluster("C8", Nil)
      val c9 = new Cluster("C9", Nil)
      val c10 = new Cluster("C10", Nil)

      val nodes = List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)

      val links = List(
        new Link(c1, c2),
        new Link(c1, c3),
        new Link(c3, c4),
        new Link(c3, c5),
        new Link(c4, c6),
        new Link(c5, c6),
        new Link(c2, c6),
        new Link(c6, c7),
        new Link(c6, c8),
        new Link(c7, c9),
        new Link(c8, c9))

      val me = List(
        new MutualExcl(c1, c3))

      new Graph(nodes, links, me)
    }

  /**
   * generates a random DAG with n vertices and m edges
   * This is probably very similar to the Erdos-Renyi model
   *
   */
  def randomDAG(n: Int, m: Int): Graph = {
    require(m <= n * (n - 1) / 2)
    var nodes = Array.ofDim[Cluster](n)
    var links = List[Link]()

    for (i <- 1 to n) {
      nodes(i - 1) = new Cluster("C" + i, Nil)
    }

    // create a list of indices. memory intensive but easier to code.
    var indices = List[(Int, Int)]()
    for (i <- 0 until n; j <- i + 1 until n) {
      indices = (i, j) :: indices
    }

    for (i <- 1 to m) {
      val pick = math.floor(math.random * indices.length).toInt // choose from the indices
      val head = indices(pick)._1
      val tail = indices(pick)._2
      links = new Link(nodes(head), nodes(tail)) :: links

      indices = indices.filterNot(_ == (head, tail))
    }

    new Graph(nodes.toList, links).compact
  }

  /**
   * generates a random DAG with n vertices, m edges, and p mutual exclusion relationships
   *
   *
   */
  def randomDAG(n: Int, m: Int, p: Int): Graph = {
    require(m <= n * (n - 1) / 2)
    var nodes = Array.ofDim[Cluster](n)
    var links = List[Link]()
    var mutex = List[MutualExcl]()

    for (i <- 1 to n) {
      nodes(i - 1) = new Cluster("C" + i, Nil)
    }

    // create a list of indices. memory intensive but easier to code.
    var possibleEdges = List[(Int, Int)]()
    for (i <- 0 until n; j <- i + 1 until n) {
      possibleEdges = (i, j) :: possibleEdges
    }

    var possibleMutex = possibleEdges

    for (i <- 1 to m) {
      val pick = math.floor(math.random * possibleEdges.length).toInt // choose from the indices
      val head = possibleEdges(pick)._1
      val tail = possibleEdges(pick)._2
      links = new Link(nodes(head), nodes(tail)) :: links

      possibleEdges = possibleEdges.filterNot(_ == (head, tail))
    }

    for (i <- 1 to p) {
      val pick = math.floor(math.random * possibleMutex.length).toInt // choose from the indices
      val head = possibleMutex(pick)._1
      val tail = possibleMutex(pick)._2
      mutex = new MutualExcl(nodes(head), nodes(tail)) :: mutex

      possibleMutex = possibleMutex.filterNot(_ == (head, tail))
    }

    new Graph(nodes.toList, links, mutex).compact
  }

  /**
   * a graph with a simple loop: 1 -> 2 -> 3 -> 1, but also contains a non-loop part: 2->4->3
   */
  val loopGraph1: Graph = {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)

    val links = List(
      new Link(c1, c2),
      new Link(c2, c3),
      new Link(c3, c1),
      new Link(c2, c4),
      new Link(c4, c3))

    new Graph(List(c1, c2, c3, c4), links)
  }

  /**
   * very similar to loopGraph1, but has no loops. 1->2->3, 2->4->3
   */
  val noloopGraph1: Graph = {
    val c1 = new Cluster("C1", Nil)
    val c2 = new Cluster("C2", Nil)
    val c3 = new Cluster("C3", Nil)
    val c4 = new Cluster("C4", Nil)

    val links = List(
      new Link(c1, c2),
      new Link(c2, c3),
      new Link(c2, c4),
      new Link(c4, c3))

    new Graph(List(c1, c2, c3, c4), links)
  }

  /**
   *
   * a graph with two loops: 0->1->2->3->0, 1->4->5->3, 4->7->3
   * two loops shared edges: 0->1, 3->0
   * addition branch: 5 -> 6
   */
  val loopGraph2: Graph = {
    val c1 = new Cluster("C0", Nil)
    val c2 = new Cluster("C1", Nil)
    val c3 = new Cluster("C2", Nil)
    val c4 = new Cluster("C3", Nil)
    val c5 = new Cluster("C4", Nil)
    val c6 = new Cluster("C5", Nil)
    val c7 = new Cluster("C6", Nil)
    val c8 = new Cluster("C7", Nil)

    val links = List(
      new Link(c1, c2),
      new Link(c2, c3),
      new Link(c3, c4),
      new Link(c4, c1),
      new Link(c2, c5),
      new Link(c5, c6),
      new Link(c6, c4),
      new Link(c5, c8),
      new Link(c8, c4),
      new Link(c6, c7))

    new Graph(List(c1, c2, c3, c4, c5, c6, c7, c8), links)
  }

  /**
   *
   * a graph with one loop: 1->2->3->1
   * addition edges: 0->1, 0->3
   */
  val loopGraph3: Graph = {
    val c1 = new Cluster("C0", Nil)
    val c2 = new Cluster("C1", Nil)
    val c3 = new Cluster("C2", Nil)
    val c4 = new Cluster("C3", Nil)

    val links = List(
      new Link(c1, c2),
      new Link(c2, c3),
      new Link(c3, c4),
      new Link(c4, c2),
      new Link(c1, c4))

    new Graph(List(c1, c2, c3, c4), links)
  }

  def main(args: Array[String]) {
    println(sample1.containsLoop())
    println(sample2.containsLoop())
    println(loopGraph1.containsLoop())
    println(noloopGraph1.containsLoop())
    println(loopGraph2.containsLoop())
    /*
      for (i <- 0 to 10) {
        val all = loopGraph2.allLoops().map(_.map(_.name))
        //println(all)
        var good = all.exists(l => l.filterNot(List("C1", "C2", "C3", "C0") contains).size == 0)
        good = good && all.exists(l => l.filterNot(List("C1", "C4", "C5", "C0", "C3") contains).size == 0)
        good = good && all.exists(l => l.filterNot(List("C1", "C4", "C7", "C0", "C3") contains).size == 0)
        good = good && all.size == 3
        println("good ? " + good)
      }
      */

    println(loopGraph3.allLoops())
  }
}

  