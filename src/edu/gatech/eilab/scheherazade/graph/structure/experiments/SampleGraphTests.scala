package edu.gatech.eilab.scheherazade.graph.structure.experiments

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.main.Global
import edu.gatech.eilab.scheherazade.graph.structure._
import edu.gatech.eilab.scheherazade.generation._

class SampleGraphTests extends FunSuite {

  test("sample Graph 6") {
    //TODO: this is like a bug in the graph traversal phase.
    val graph = SampleGraph.sample6
    //graph.draw("aaaa")
    val background = graph.nodes(5)
    val queryCluster = graph.nodes(1)
    testGraph(graph, List(background), queryCluster)
  }

  def testGraph(graph: Graph, bgList: List[Cluster], q: Cluster) {
    //edu.gatech.eilab.scheherazade.graph.passage.Passage.debug = true
    //val oldSeqs = ExhaustiveSeqGen.interactiveGenerate(graph)
    val oldSeqs = ExhaustiveSeqGen.generate(graph)

    val oldValid = oldSeqs.filter(seq => bgList.forall(seq.contains))
    val oldQueried = oldValid.filter(seq => seq.contains(q))
    val oldRatio = oldQueried.size * 1.0 / oldValid.size
    println("# old seqs " + oldSeqs.size + " ratio = " + oldRatio)

    val (newGraph, newSeqs) = Main.performAnalysis(graph, bgList, q)
    val newValid = newSeqs.filter(seq => bgList.forall(seq.contains))
    val newQueried = newValid.filter(seq => seq.contains(q))
    val newRatio = newQueried.size * 1.0 / newValid.size
    println("# new seqs " + newSeqs.size + " ratio = " + newRatio)

    if (newRatio != oldRatio) {

      graph.graphWithOptionalsAndSkips.draw("mutex-old")
      newGraph.draw("mutex-new")

      println("background = " + bgList.map(_.name).mkString(" "))
      println("query = " + q.name)

      val missing = oldValid.filterNot(newValid.contains)
      val extra = newValid.filterNot(oldValid.contains)
      println("missing seqs:")
      printSeqs(missing)
      println("extra seqs")
      printSeqs(extra)
    }
    assert(oldRatio == newRatio)

  }

  def printSeqs(seqs: List[List[Cluster]]) {
    for (seq <- seqs) {
      println(seq.map(_.name).mkString(" "))
    }
  }

  test("Mutual Exclusion Analysis on Sample Graph 7") {
    val graph = SampleGraph.sample7
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(2)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 8") {
    val graph = SampleGraph.sample8
    val background = graph.nodes(1)
    val queryCluster = graph.nodes(8)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 9") {
    val graph = SampleGraph.sample9
    val background = graph.nodes(0)
    val queryCluster = graph.nodes(1)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 11") {
    val graph = SampleGraph.sample11
    val background = graph.nodes(1)
    val queryCluster = graph.nodes(5)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 10") {
    val graph = SampleGraph.sample10
    val background = graph.nodes(4)
    val queryCluster = graph.nodes(5)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 12-1") {
    val graph = SampleGraph.sample12
    val background = graph.nodes(2)
    val queryCluster = graph.nodes(10)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 12-2") {
    val graph = SampleGraph.sample12
    val background = graph.nodes(1)
    val queryCluster = graph.nodes(5)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 13") {
    val graph = SampleGraph.sample13
    val background = graph.nodes(3)
    val queryCluster = graph.nodes(9)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 14") {
    val graph = SampleGraph.sample14
    val background = graph.nodes(9)
    val queryCluster = graph.nodes(4)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 15") {
    val graph = SampleGraph.sample15
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(5)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 16") {
    // A race condition between C3 and C6, which are ordered. 
    // Since C6 is ordered after C3, the contended node cannot be deleted
    val graph = SampleGraph.sample16
    //graph.draw("sample16")
    val background = graph.nodes(5)
    val queryCluster = graph.nodes(1)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 17") {
    val graph = SampleGraph.sample17
    val background = graph.nodes(4)
    val queryCluster = graph.nodes(7)
    testGraph(graph, List(background), queryCluster)
  }
  //
  test("Mutual Exclusion Analysis on Sample Graph 18") {
    val graph = SampleGraph.sample18
    val background = graph.nodes(5)
    val queryCluster = graph.nodes(1)
    testGraph(graph, List(background), queryCluster)
  }
  //
  test("Mutual Exclusion Analysis on Sample Graph 19") {
    val graph = SampleGraph.sample19
    val background = graph.nodes(4)
    val queryCluster = graph.nodes(8)
    testGraph(graph, List(background), queryCluster)
  }
  //
  test("Mutual Exclusion Analysis on Sample Graph 20") {
    val graph = SampleGraph.sample20
    val background = graph.nodes(2)
    val queryCluster = graph.nodes(3)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 21") {
    val graph = SampleGraph.sample21
    val background = graph.nodes(6)
    val queryCluster = graph.nodes(1)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 22") {
    val graph = SampleGraph.sample22
    val background = graph.nodes(8)
    val queryCluster = graph.nodes(7)
    testGraph(graph, List(background), queryCluster)
  }
  //
  test("Mutual Exclusion Analysis on Sample Graph 23") {
    val graph = SampleGraph.sample23
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(8)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 24") {
    val graph = SampleGraph.sample24
    val background = graph.nodes(8)
    val queryCluster = graph.nodes(0)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 25") {
    // a race condition between a kept node and an optional node. 
    // The optional node is not precondition for anything, so it is not a precondition for children of deleted node.
    val graph = SampleGraph.sample25
    val background = graph.nodes(5)
    val queryCluster = graph.nodes(1)
    testGraph(graph, List(background), queryCluster)
  }
  //
  test("Mutual Exclusion Analysis on Sample Graph 26") { // this is the creation of recursiveDeleted
    val graph = SampleGraph.sample26
    val background = graph.nodes(4)
    val queryCluster = graph.nodes(8)
    testGraph(graph, List(background), queryCluster)
  }
  //
  test("Mutual Exclusion Analysis on Sample Graph 27") {
    /**
     * this is a test case for adding temporal relations
     *
     */
    val graph = SampleGraph.sample27
    val background = graph.nodes(5)
    val queryCluster = graph.nodes(3)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 28") { // putting recursiveDeleted at the right place, i.e. the last step
    val graph = SampleGraph.sample28
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(5)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 29") {
    // this is a test for race conditions concerning kid deletion
    val graph = SampleGraph.sample29
    val background = graph.nodes(5)
    val queryCluster = graph.nodes(6)
    testGraph(graph, List(background), queryCluster)
  }
  //
  test("Mutual Exclusion Analysis on Sample Graph 30") {
    val graph = SampleGraph.sample30
    val background = graph.nodes(2)
    val queryCluster = graph.nodes(4)
    testGraph(graph, List(background), queryCluster)
  }
  //
  test("Mutual Exclusion Analysis on Sample Graph 31") {
    // This test shows we cannot add new links for skipping optional events after mutual analysis deletes events
    val graph = SampleGraph.sample31
    val background = graph.nodes(4)
    val queryCluster = graph.nodes(3)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 32") {
    val graph = SampleGraph.sample32
    val background = graph.nodes(2)
    val queryCluster = graph.nodes(0)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 33") {
    val graph = SampleGraph.sample33
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(2)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 34") {
    val graph = SampleGraph.sample34
    val background = graph.nodes(8)
    val queryCluster = graph.nodes(2)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 35") {
    val graph = SampleGraph.sample35
    val background = graph.nodes(1)
    val queryCluster = graph.nodes(7)
    testGraph(graph, List(background), queryCluster)
  }

  test("Mutual Exclusion Analysis on Sample Graph 36") {
    val graph = SampleGraph.sample36
    val background = graph.nodes(8)
    val queryCluster = graph.nodes(1)
    testGraph(graph, List(background), queryCluster)
  }

}