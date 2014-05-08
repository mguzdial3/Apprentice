package edu.gatech.eilab.scheherazade.graph.structure
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer


object AnalysisMain {
  
  def main(args: Array[String]) {
    //val graph = SampleGraph.randomDAG(15, 60, 3)
    val graph = SampleGraph.sample5
    graph.draw("random")
    val (covens, closures) = UnitAnalysis.analyzeUnit(graph)
    println("always togethers (" + covens.size + "): " + covens.mkString(", \n"))
    println("independent (" + closures.size + "): " + closures.mkString(", \n"))
  }

}