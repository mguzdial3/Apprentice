package edu.gatech.eilab.scheherazade.graph.structure

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.graph._

class UnitTests extends FunSuite {

  test("Mutual Exclusion Analysis on Sample Graph 6") {
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
    before.draw("before")
    val graph = AnalysisMain.regularize(before)
    graph.draw("after")
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
    before.draw("before")
    val graph = AnalysisMain.regularize(before)
    graph.draw("after")
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
    before.draw("before")
    val graph = AnalysisMain.regularize(before)
    graph.draw("after")
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
    before.draw("before")
    val graph = AnalysisMain.regularize(before)
    graph.draw("after")
    val background = graph.nodes(2)
    val queryCluster = graph.nodes(12)

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
}