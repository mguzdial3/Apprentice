package edu.gatech.eilab.scheherazade.graph.structure
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object AnalysisMain {

  def main(args: Array[String]) {
    testAll
  }

  /**
   * test out all simplification techniques
   *
   */
  def testAll() {
    var graph = SampleGraph.randomDAG(20, 70, 4).graphWithOptionalsAndSkips
    graph.draw("unit-analysis")
    
    //TODO: add a pseudo start event
    
    var clans = UnitAnalysis.findClans(graph)
    println("clans = " + clans.mkString(", "))
    val meNodes = graph.mutualExcls.flatMap(me => List(me.c1, me.c2)).distinct
    val rndInd = math.floor(math.random * meNodes.size).toInt
    val background = meNodes(rndInd)
    println("background = " + background.name)
    
    graph = MutexAnalysis.cleanedGraph(graph, List(background), clans)
    graph.draw("mutex-analysis")
    
    clans = UnitAnalysis.findClans(graph)
    graph = UnitAnalysis.collapseClans(graph, clans)
    graph.draw("after-collapsing")

    val closures = UnitAnalysis.findClosures(graph)
    println(closures.mkString(", "))
  }

  def testClans() {
    import java.io._
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("random record.csv")))

    for (nodes <- 10 to 20) {
      val all = nodes * (nodes - 1) / 2
      val least = math.floor(all * 0.5).toInt
      val most = math.floor(all * 0.6667).toInt
      for (links <- least to most) {
        var total = 0
        var numNodes = 0
        for (i <- 1 to 100) {
          val graph = SampleGraph.randomDAG(nodes, links, 3)
          //val graph = SampleGraph.sample5
          //graph.draw("random")
          val clans = UnitAnalysis.findClans(graph)
          total += clans.size
          numNodes += clans.map(_.size).sum
        }

        //pw.println(nodes + ", " + links + ", " + total/100.0 + ", " + numNodes / 100.0)
        pw.println(nodes + ", " + links.toDouble / all + ", " + numNodes / 100.0 / nodes)
        println(nodes + ", " + links + ", " + total / 100.0 + ", " + numNodes / 100.0)
        println(nodes + ", " + links.toDouble / all + ", " + numNodes / 100.0 / nodes)
      }
    }

    pw.close

  }

}