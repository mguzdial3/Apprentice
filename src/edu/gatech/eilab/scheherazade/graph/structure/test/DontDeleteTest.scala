package edu.gatech.eilab.scheherazade.graph.structure.test

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.structure._

class DontDeleteTest extends FunSuite {

  test("Dont Delete Test on Sample Graph 18") {
    val graph = SampleGraph.sample18.graphWithOptionalsAndSkips
    val kept = List(graph.nodes(5))
    val q = graph.nodes(2)
    println(kept)
    val (causes, deleted, recursiveDeletion) = MutexAnalysis.causeForDeletionNew(graph, kept)
    // check for cause of deletion
    val cq = causes(q)
    assert(cq.contains(Set(graph.nodes(5))) && cq.contains(Set(graph.nodes(0))))
    val ngraph = MutexAnalysis.cleanNodesNew(graph, kept)
    assert(ngraph.nodes.contains(q))
  }
}