package edu.gatech.eilab.scheherazade.graph.structure.experiments

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.sample._

class MiscTest extends FunSuite {

  test("Equality in RaceCondition") {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)

    val cond1 = new RaceCondition(a, List(a, b), List(c, d))
    val cond2 = new RaceCondition(a, List(c, d), List(a, b))
    println(cond1)
    println(cond2)
    assert(cond1 == cond2)

    val cond3 = new RaceCondition(a, List(c, a), List(d, b))

    assert(cond1 != cond3)

    val cond4 = new RaceCondition(a, List(a), List(c))
    val cond5 = new RaceCondition(a, List(c), List(a))
    
    val cond6 = new RaceCondition(a, List(new Cluster("c", Nil)), List(new Cluster("a", Nil)))
    
    assert(cond4 == cond5)
    assert(cond4 == cond6)
    
    assert(List(cond1, cond2).distinct.size == 1)
  }

}