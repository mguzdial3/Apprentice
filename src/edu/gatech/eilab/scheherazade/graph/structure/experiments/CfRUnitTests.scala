package edu.gatech.eilab.scheherazade.graph.structure.experiments

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.sample._

class CfRUnitTests extends FunSuite {
  test("Sample Graph 1") {
    val graph = CfRSample.graph1
    val result = CfRComputer.processGraph(graph)
    val f = result(new Cluster("f", Nil))
    //    println(f)
    assert(f.size == 1 && f.contains(List(new Cluster("d", Nil))))
  }

  test("Sample Graph 2") {
    val graph = CfRSample.graph2
    val result = CfRComputer.processGraph(graph)
    val f = result(new Cluster("f", Nil))
    //    println(f)
    assert(f.size == 1 && f.contains(List(new Cluster("d", Nil), new Cluster("e", Nil))))
  }

  test("Sample Graph 3") {
    val graph = CfRSample.graph3
    val result = CfRComputer.processGraph(graph)
    val i = result(new Cluster("i", Nil))
    //    println(i)
    assert(i.size == 1 && containsCause(i, List(new Cluster("d", Nil), new Cluster("e", Nil), new Cluster("h", Nil))))
  }

  test("Sample Graph 4") {
    val graph = CfRSample.graph4
    val result = CfRComputer.processGraph(graph)
    val f = result(new Cluster("h", Nil))
    //    println(f)
    assert(f.size == 2 && containsCause(f, List(new Cluster("d", Nil), new Cluster("e", Nil))) &&
      f.contains(List(new Cluster("g", Nil))))
  }

  test("Sample Graph 5") {
    val graph = CfRSample.graph5
    val result = CfRComputer.processGraph(graph)
    val f = result(new Cluster("i", Nil))
    //    println(f)
    assert(f.size == 1 && containsCause(f, List(new Cluster("d", Nil), new Cluster("e", Nil), new Cluster("h", Nil))))
  }

  test("Sample Graph 6") {
    val graph = CfRSample.graph6
    val result = CfRComputer.processGraph(graph)
    val f = result(new Cluster("i", Nil))
    //    println(f)
    assert(f.size == 2 && containsCause(f, List(new Cluster("d", Nil), new Cluster("e", Nil), new Cluster("h", Nil))) &&
      containsCause(f, List(new Cluster("d", Nil), new Cluster("l", Nil), new Cluster("h", Nil))))
  }

  test("Sample Graph 7") {
    val graph = CfRSample.graph7
    val result = CfRComputer.processGraph(graph)
    val f = result(new Cluster("i", Nil))
    println(f)
    assert(f.size == 1 && containsCause(f, List(new Cluster("d", Nil), new Cluster("l", Nil), new Cluster("h", Nil))))
  }

  def containsCause(allCauses: List[List[Cluster]], oneCause: List[Cluster]): Boolean =
    {
      allCauses.exists(x => x.forall(oneCause.contains) && oneCause.forall(x.contains))
    }

}