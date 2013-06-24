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

  