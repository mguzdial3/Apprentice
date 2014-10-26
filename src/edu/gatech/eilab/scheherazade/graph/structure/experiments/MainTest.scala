package edu.gatech.eilab.scheherazade.graph.structure.experiments

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.sample._

class MainTest extends FunSuite {

  test("sequence detection") {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)

    val seq = List(a, b, c, d, e, f)
    val seq2 = List(a, b, c, e, d, f)
    val seq3 = List(a, f, b, c, d, e)
    val seq4 = List(a, c, d, e)
    val seq5 = List(a, e, c, d, f)

    val condprec = new ConditionalPrec(List(a, b, c, d), e)

    assert(Main.meetsCondition(seq, List(condprec)))
    assert(!Main.meetsCondition(seq2, List(condprec)))
    assert(Main.meetsCondition(seq3, List(condprec)))
    assert(Main.meetsCondition(seq4, List(condprec)))
    assert(Main.meetsCondition(seq5, List(condprec)))
  }

  test("sample Graph 6") {
    val graph = SampleGraph.sample6
    val background = graph.nodes(7)
    val queryCluster = graph.nodes(3)
    //    testGraph(graph, List(background), queryCluster)
  }
}