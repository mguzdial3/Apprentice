package edu.gatech.eilab.scheherazade.graph

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.io._
import edu.gatech.eilab.scheherazade.graph._

import breeze.linalg._

/**
 * Performs the PageRank algorithm on plot graphs
 *
 *  @author Albert Li
 *  @date Jun 24 2013
 */
object PageRank {

  def main(args: Array[String]) {

    //var graph = SampleGraph.sample1 //getGraph()
    var originalGraph = getGraph()
    //originalGraph.draw("./pagerank")

    val graph = loopFromBottom(originalGraph)
    val n = graph.nodes.length
    var matrix = simpleTransitionMatrix(graph)
    //println(matrix.toString(200, 200))

    val uniform = uniformProbMatrix(n)
    val weightTransition = freqWeightMatrix(graph)
    val mutexTransition = freqMtlExMatrix(graph)

    //print(mutexTransition.toString(300,300))
    matrix = 0.7 * matrix + 0.3 * mutexTransition //mutexTransition // + 0.005 * uniform 

    //println(matrix.toString(200, 200))
    val t1 = System.currentTimeMillis()
    val (a, b, c) = eig(matrix)
    val t2 = System.currentTimeMillis()
    println("computation time = " + (t2 - t1))

    //println("a = " + a)
    //println("a = " + c.toString(200, 200))
    var events = List[(String, Double)]()

    var found = false
    for (i <- 0 until n) {
      //println(a.valueAt(i))

      val value = a.valueAt(i)
      if (value >= 0.9999 && value <= 1.0001) {
        println("Successfully found stationary eigen distribution.")
        found = true
        for (j <- 0 until n) {
          //print("weight for " + graph.nodes(j).name + ": ")
          //println(c(j, i))

          events = (graph.nodes(j).name, math.abs(c(j, i))) :: events
        }
      }
    }

    if (!found) {
      println("Failed to find stationary eigen distribution.")
      throw new ArithmeticException("Failed to find stationary eigen distribution.")
    }

    drawRankedGraph(events, originalGraph)

    Thread.sleep(5000)
  }

  def freqMtlExMatrix(graph: Graph): DenseMatrix[Double] =
    {
      val nodes = graph.nodes
      val n = nodes.length
      val matrix = new DenseMatrix[Double](n, n)

      val freq = Array.ofDim[Int](n)

      for (i <- 0 until n) {
        freq(i) = nodes(i).members.length
        for (j <- 0 until n) {
          matrix(i, j) = freq(i)
        }
      }

      var total = Array.fill[Double](n)(0.0)
//      for (j <- 0 until n; i <- 0 until n) {
//        total(j) += matrix(i, j)
//      }

      //      for (melink <- graph.mutualExcls) {
      //        val i = nodes.indexWhere(v => v == melink.c1)
      //        val j = nodes.indexWhere(v => v == melink.c2)
      //        matrix(i, j) = -0.5 * total(i) //-0.1 * math.min(total(i), total(j))
      //        matrix(j, i) = -0.5 * total(j) //-0.1 * math.min(total(i), total(j))
      //      }

      for (i <- 0 until n) {
        var list = List[Int]()
        
        // add the mutually exclusive vertices to list
        for (melink <- graph.mutualExcls) {          
          if (nodes(i) == melink.c1) {
            val j = nodes.indexWhere(v => v == melink.c2)
            list = j :: list
          } else if (nodes(i) == melink.c2) {
            val j = nodes.indexWhere(v => v == melink.c1)
            list = j :: list
          }
        }

        val cnt = list.length // size of the exclusion list for i

        for (j <- ((0 until n) filterNot (list contains)))
        {
          total(i) += matrix(j, i) // sum of i's transition to other nodes not in the exclusion list
        }
        
        for (j <- list) {
          matrix(j, i) = -0.5 * total(i) / cnt 
          // distribute some negative power, equal to the rest of the outgoing power, 
          // to all nodes in the exclusion list
        }

      }

      // matrix normalization
      total = Array.fill[Double](n)(0.0)
      for (j <- 0 until n; i <- 0 until n) {
        total(j) += matrix(i, j)
      }

      for (j <- 0 until n) {
        for (i <- 0 until n) {
          matrix(i, j) = matrix(i, j) / total(j)
        }
      }

      matrix
    }

  def freqWeightMatrix(graph: Graph): DenseMatrix[Double] =
    {
      val nodes = graph.nodes
      val n = nodes.length

      val freq = Array.ofDim[Int](n)
      var total = 0.0

      for (i <- 0 until n) {
        freq(i) = nodes(i).members.length
        total += freq(i)
      }

      if (total == 0) throw new ArithmeticException("Total frequency equals zero. This will leads to division by zero.")

      val matrix = new DenseMatrix[Double](n, n)

      for (i <- 0 until n; j <- 0 until n) {
        matrix(i, j) = freq(i) / total
      }

      matrix
    }

  def drawRankedGraph(events: List[(String, Double)], graph: Graph) {

    val g = new Graph(graph.nodes, graph.links)
    // find median
    val allweights = events.map(_._2).sortWith(_ > _)
    val n = allweights.length
    println("n = " + n)

    val median =
      if (n % 2 == 0) {
        // n is even
        (allweights(n / 2) + allweights(n / 2 + 1)) / 2.0
      } else {
        // n is odd
        allweights(n / 2)
      }

    //    var smallest = 1.0
    //
    //    //println(events)
    //    events foreach { pair =>
    //      if (pair._2 < smallest) {
    //        smallest = pair._2
    //      }
    //    }

    val map = g.nodes.map {
      node =>
        val score = events.find(pair => pair._1 == node.name).get._2
        val newName = node.name + "\t" + ("%1.2f" format (score / median)) // median normalized to 1
        println(newName)
        node -> newName        
    }.toMap

    g.drawWithNames("./pagerank", map)
  }

  /**
   * Adds links from the sink nodes to the source nodes.
   *  That is, looping from the bottom of the graph to the top of the graph
   */
  def loopFromBottom(graph: Graph): Graph =
    {
      val tops = graph.nodes.filter { n =>
        !graph.links.exists(l => l.target == n)
      }

      val bottoms = graph.nodes.filter { n =>
        !graph.links.exists(l => l.source == n)
      }

      val newlinks = for (n1 <- tops; n2 <- bottoms) yield new Link(n2, n1)

      new Graph(graph.nodes, newlinks ::: graph.links, graph.mutualExcls)
    }

  /**
   * Add the damping factor in PageRank to the outdegree matrix
   *
   */
  def uniformProbMatrix(n: Int): DenseMatrix[Double] =
    {
      DenseMatrix.fill(n, n)(1.0 / n)
    }

  /**
   * builds a simple transition matrix based on a graph
   *  For each node i, d = its outgoing degree
   *  For a link going from i to j, matrix (j)(i) = 1/d
   */
  def simpleTransitionMatrix(graph: Graph): DenseMatrix[Double] =
    {
      val n = graph.nodes.length
      val matrix = DenseMatrix.fill[Double](n, n)(0.0)

      for (i <- 0 until n) {
        // tail of the temporal link
        val tail = graph.nodes(i)

        // for each outgoing link, record the heads
        val outgoing = (0 until n).filter {
          j => graph.links.exists(link => link.source == tail && link.target == graph.nodes(j))
        }

        // out degree
        val degree = outgoing.length

        for (j <- outgoing) {
          matrix(j, i) = 1.0 / degree
        }
      }

      matrix
    }

  /**
   * Generates a plot graph
   *
   */
  def getGraph(): Graph =
    {
      val reader = new ConfigReader("configNewMvBest.txt")
      //var (stories, clusters) = reader.initDataFiltered()
      var (stories, clusters) = reader.initData()
      val para = reader.properties.allParameters()(0)

      val minimumSize = para.intParam("minClusterSize")
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      val gen = new GraphGenerator(insideStories, insideClusters)
      var graph: Graph = gen.oldGenerate(para)("mutualExcl")._1

      graph
    }
}