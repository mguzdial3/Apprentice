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

    
    var graph = SampleGraph.sample1 //getGraph()
    graph = loopFromBottom(graph)
    val n = graph.nodes.length
    var matrix = simpleTransitionMatrix(graph)
    //println(matrix.toString(200, 200))
    //graph.draw("./test1")
    //Thread.sleep(5000)
    matrix = addDamping(matrix, 0.15)
    //println(matrix.toString(200, 200))
    val t1 = System.currentTimeMillis()
    val (a, b, c) = eig(matrix)
    val t2 = System.currentTimeMillis()
    println("computation time = " + (t2 - t1))
    
    for (i <- 0 until n)
    {
      //println(a.valueAt(i))
      if (a.valueAt(i) >= 0.9999)
      {
        for (j <- 0 until n)
        {
          print("weight for " + graph.nodes(j).name + ": ")
          println(c(j, i))
        }
      }
    }
//    println(a)
//    println(b)
//    println(c.toString(300, 300))

  }
  
  /** Adds links from the sink nodes to the source nodes.
   *  That is, looping from the bottom of the graph to the top of the graph
   */
  def loopFromBottom(graph:Graph):Graph =
  {
    val tops = graph.nodes.filter{n =>
      !graph.links.exists(l => l.target == n)
      }
    
    val bottoms = graph.nodes.filter{n =>
      !graph.links.exists(l => l.source == n)
      }
    
    val newlinks = for (n1 <- tops; n2 <- bottoms) yield new Link(n2, n1)
    
    new Graph(graph.nodes, newlinks ::: graph.links)
  }
  
  /** Add the damping factor in PageRank to the outdegree matrix
   *  
   */
  def addDamping(matrix:DenseMatrix[Double], damping:Double) =
  {
    val n = matrix.rows
    matrix * (1 - damping) + DenseMatrix.fill(n,n)(damping/n) 
  }
  
  /** builds a simple transition matrix based on a graph
   *  For each node i, d = its outgoing degree
   *  For a link going from i to j, matrix (j)(i) = 1/d
   */
  def simpleTransitionMatrix(graph:Graph):DenseMatrix[Double] =
  {
    val n = graph.nodes.length
    val matrix = DenseMatrix.fill[Double](n, n)(0.0)
    
    for (i <- 0 until n)
    {
      // tail of the temporal link
      val tail = graph.nodes(i)
      
      // for each outgoing link, record the heads
      val outgoing = (0 until n).filter{
        j => graph.links.exists(link => link.source == tail && link.target == graph.nodes(j))
      }
      
      // out degree
      val degree = outgoing.length
      
      for (j <- outgoing)
      {
        matrix(j, i) = 1.0 / degree
      }
    }
    
    matrix
  }

  /** Generates a plot graph
   *  
   */
  def getGraph(): Graph =
    {
      val reader = new ConfigReader("configRobBest.txt")
      var (stories, clusters) = reader.initDataFiltered()

      val para = reader.properties.allParameters()(0)

      val minimumSize = para.intParam("minClusterSize")
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      val gen = new GraphGenerator(insideStories, insideClusters)
      var graph: Graph = gen.generate(para)("improved")._1
      
      graph
    }
}