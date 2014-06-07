package edu.gatech.eilab.scheherazade.graph.structure.test

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph.structure.MutexAnalysisNew

class BooleanSimplifyTest extends FunSuite {

  test("Test simplifying boolean formula 1") {
    val test = List(Set(new Cluster("c1", Nil), new Cluster("c2", Nil), new Cluster("c3", Nil)), Set(new Cluster("c3", Nil)), Set(new Cluster("c4", Nil)), Set(new Cluster("c4", Nil), new Cluster("c6", Nil)))
    val result = MutexAnalysisNew.simplifyBoolean(test)
    println(result)
    val correct = List(Set(new Cluster("c3", Nil)), Set(new Cluster("c4", Nil)))
    assert(correct.forall(result.contains) && result.forall(correct.contains))
  }

  test("Test simplifying boolean formula 2") {
    val test = List(Set(new Cluster("c1", Nil), new Cluster("c2", Nil), new Cluster("c3", Nil)), Set(new Cluster("c3", Nil)), Set(new Cluster("c3", Nil), new Cluster("c4", Nil)))
    val result = MutexAnalysisNew.simplifyBoolean(test)
    println(result)
    val correct = List(Set(new Cluster("c3", Nil)))
    assert(correct.forall(result.contains) && result.forall(correct.contains))
  }

}