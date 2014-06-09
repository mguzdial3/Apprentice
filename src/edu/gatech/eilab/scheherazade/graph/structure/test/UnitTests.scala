package edu.gatech.eilab.scheherazade.graph.structure.test

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.main.Global
import edu.gatech.eilab.scheherazade.graph.structure._

class UnitTests extends FunSuite {

  test("Mutual Exclusion Analysis on Sample Graph 6") {
    Global.graphDrawing = false
    val before = SampleGraph.sample6
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(3)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 7") {
    val before = SampleGraph.sample7
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(9)
    val queryCluster = graph.nodes(4)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 8") {
    val before = SampleGraph.sample8
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(3)
    val queryCluster = graph.nodes(10)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 9") {
    val before = SampleGraph.sample9
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(2)
    val queryCluster = graph.nodes(3)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 11") {
    val before = SampleGraph.sample11
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(3)
    val queryCluster = graph.nodes(7)

    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 10") {
    val before = SampleGraph.sample10
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(6)
    val queryCluster = graph.nodes(7)

    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 12-1") {
    val before = SampleGraph.sample12
    val graph = AnalysisMain.regularize(before)
    graph.draw("sample12")
    val background = graph.nodes(4)
    val queryCluster = graph.nodes(12)

    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 12-2") {
    val before = SampleGraph.sample12
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(3)
    val queryCluster = graph.nodes(7)

    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 13") {

    val before = SampleGraph.sample13
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(5)
    val queryCluster = graph.nodes(11)
    graph.draw("sample13")
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 14") {
    val before = SampleGraph.sample14
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(11)
    val queryCluster = graph.nodes(6)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 15") {
    val before = SampleGraph.sample15
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(9)
    val queryCluster = graph.nodes(7)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 16") {
    // A race condition between C3 and C6, which are ordered. 
    // Since C6 is ordered after C3, the contended node cannot be deleted
    val before = SampleGraph.sample16
    val graph = AnalysisMain.regularize(before)
    graph.draw("sample16")
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(3)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 17") {
    val before = SampleGraph.sample17
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(6)
    val queryCluster = graph.nodes(9)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 18") {
    val before = SampleGraph.sample18
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(3)
    graph.draw("sample18")
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 19") {
    val before = SampleGraph.sample19
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(6)
    val queryCluster = graph.nodes(10)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 20") {
    val before = SampleGraph.sample20
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(4)
    val queryCluster = graph.nodes(5)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 21") {
    val before = SampleGraph.sample21
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(8)
    val queryCluster = graph.nodes(3)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 22") {
    val before = SampleGraph.sample22
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(10)
    val queryCluster = graph.nodes(9)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 23") {
    val before = SampleGraph.sample23
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(9)
    val queryCluster = graph.nodes(10)
    graph.draw("sample23")
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 24") {
    val before = SampleGraph.sample24
    val graph = AnalysisMain.regularize(before)

    val background = graph.nodes(10)
    val queryCluster = graph.nodes(2)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 25") {
    // a race condition between a kept node and an optional node. 
    // The optional node is not precondition for anything, so it is not a precondition for children of deleted node.
    val before = SampleGraph.sample25
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(3)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 26") { // this is the creation of recursiveDeleted
    val before = SampleGraph.sample26
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(6)
    val queryCluster = graph.nodes(10)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 27") {
    /**
     * this is a test case for adding temporal relations
     *
     */
    val before = SampleGraph.sample27
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(5)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 28") { // putting recursiveDeleted at the right place, i.e. the last step
    val before = SampleGraph.sample28
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(9)
    val queryCluster = graph.nodes(7)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

  test("Mutual Exclusion Analysis on Sample Graph 29") {
    // this is a test for race conditions concerning kid deletion
    val before = SampleGraph.sample29
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(8)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }
  
    test("Mutual Exclusion Analysis on Sample Graph 30") {
    val before = SampleGraph.sample30
    val graph = AnalysisMain.regularize(before)
    val background = graph.nodes(4)
    val queryCluster = graph.nodes(6)
    println(background)
    println(queryCluster)
    println("optionals = " + graph.optionals)
    println("conditionals = " + graph.conditionals)
    val (cGraph, sGraph) = AnalysisMain.simplifyGraph(graph, List(background))
    val (originalTotal, originalGood, originalQuery) = AnalysisMain.countStories(graph, List(background), List(queryCluster))
    //println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)
    val (cleanTotal, cleanGood, cleanQuery) = AnalysisMain.countStories(cGraph, List(background), List(queryCluster))
    //println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    assert(originalQuery.toDouble / originalGood == cleanQuery.toDouble / cleanGood)
  }

}