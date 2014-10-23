package edu.gatech.eilab.scheherazade.graph.structure.experiments

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.sample._

class CfRUnitTests extends FunSuite {
  test("Sample Graph 1") {
    val graph = CfRSample.graph1
    val answer = CfRComputer.processGraph(graph)
    val result = answer._1
    val f = result(new Cluster("f", Nil))
    assert(f.size == 1 && f.contains(List(new Cluster("d", Nil))))
    val raceCond = answer._2    
    assert(raceCond.size == 1 && raceCond.contains(new RaceCondition(new Cluster("f", Nil), List(new Cluster("d", Nil)), List(new Cluster("e", Nil)))))
    println("race conditions = " + raceCond)
  }

  test("Sample Graph 2") {
    val graph = CfRSample.graph2
    val result = CfRComputer.processGraph(graph)._1
    val f = result(new Cluster("f", Nil))
    assert(f.size == 1 && f.contains(List(new Cluster("d", Nil), new Cluster("e", Nil))))
  }

  test("Sample Graph 3") {
    val graph = CfRSample.graph3
    val result = CfRComputer.processGraph(graph)._1
    val i = result(new Cluster("i", Nil))
    assert(i.size == 1 && containsCause(i, List(new Cluster("d", Nil), new Cluster("e", Nil), new Cluster("h", Nil))))
  }

  test("Sample Graph 4") {
    val graph = CfRSample.graph4
    val result = CfRComputer.processGraph(graph)._1
    val f = result(new Cluster("h", Nil))
    assert(f.size == 2 && containsCause(f, List(new Cluster("d", Nil), new Cluster("e", Nil))) &&
      f.contains(List(new Cluster("g", Nil))))
  }

  test("Sample Graph 5") {
    val graph = CfRSample.graph5
    val result = CfRComputer.processGraph(graph)._1
    val f = result(new Cluster("i", Nil))
    assert(f.size == 1 && containsCause(f, List(new Cluster("d", Nil), new Cluster("e", Nil), new Cluster("h", Nil))))
  }

  test("Sample Graph 6") {
    val graph = CfRSample.graph6
    val result = CfRComputer.processGraph(graph)._1
    val f = result(new Cluster("i", Nil))
    assert(f.size == 2 && containsCause(f, List(new Cluster("d", Nil), new Cluster("e", Nil), new Cluster("h", Nil))) &&
      containsCause(f, List(new Cluster("d", Nil), new Cluster("l", Nil), new Cluster("h", Nil))))
  }

  test("Sample Graph 7") {
    val graph = CfRSample.graph7
    val answer = CfRComputer.processGraph(graph)
    val cfr = answer._1 
    val f = cfr(new Cluster("i", Nil))
    assert(f.size == 1 && containsCause(f, List(new Cluster("d", Nil), new Cluster("l", Nil), new Cluster("h", Nil))))
    val raceCond = answer._2 
    println(raceCond)    
    assert(raceCond.contains(new RaceCondition((new Cluster("i", Nil)), List(new Cluster("d", Nil), new Cluster("e", Nil)), List(new Cluster("h", Nil)))))
  }

  test("Sample Graph 8") {
    val graph = CfRSample.graph8
    val result = CfRComputer.processGraph(graph)._1
    val f = result(new Cluster("g", Nil))
    assert(f.size == 1 && containsCause(f, List(new Cluster("d", Nil))))
  }

  test("Sample Graph 9") {
    val graph = CfRSample.graph9
    val result = CfRComputer.processGraph(graph)._1
    val f = result(new Cluster("e", Nil))
    assert(f.size == 1 && containsCause(f, List(new Cluster("c", Nil))))
  }

  test("Sample Graph 10") {
    val graph = CfRSample.graph10
    val result = CfRComputer.processGraph(graph)._1
    val f = result(new Cluster("f", Nil))
    assert(f.size == 0)
  }

  test("Sample Graph 11") {
    val graph = CfRSample.graph11
    val answer = CfRComputer.processGraph(graph)
    val map = answer._1
    println(CfRComputer.formatMap(map))
    val d = map(new Cluster("d", Nil))
    assert(d.size == 2 && d.contains(List(new Cluster("g", Nil))) && d.contains(List(new Cluster("f", Nil))))
    val e = map(new Cluster("e", Nil))
    assert(e.size == 1 && e.contains(List(new Cluster("g", Nil))))
    val raceCond = answer._2
    println(raceCond)
    assert(raceCond.size == 1 && raceCond.contains(new RaceCondition(new Cluster("e", Nil), List(new Cluster("f", Nil)), List(new Cluster("g", Nil)))))
  }

  test("Sample Graph 12") {
    val graph = CfRSample.graph12
    graph.draw("aaaa")
    val answer = CfRComputer.processGraph(graph)
    val map = answer._1
    assert(map(new Cluster("i", Nil)) == Nil)
    val raceCond = answer._2
    assert(raceCond == Nil)
    //println("races = " + raceCond)
  }

  test("Sample Graph 13") {
    val graph = CfRSample.graph13
    graph.draw("aaaa")
    val answer = CfRComputer.processGraph(graph)
    val map = answer._1
    println("cfr = " + CfRComputer.formatMap(map))
    val raceCond = answer._2
    println("races = " + raceCond)
  }
  
    test("Sample Graph 14") {
    val graph = CfRSample.graph14
    graph.draw("aaaa")
    val answer = CfRComputer.processGraph(graph)
    val map = answer._1
    println("cfr = " + CfRComputer.formatMap(map))
    val raceCond = answer._2
    println("races = " + raceCond)
  }

  def containsCause(allCauses: List[List[Cluster]], oneCause: List[Cluster]): Boolean =
    {
      allCauses.exists(x => x.forall(oneCause.contains) && oneCause.forall(x.contains))
    }

  def containsRaceCond(raceList: List[(List[Cluster], List[Cluster])], answer: List[(List[Cluster], List[Cluster])]): Boolean =
    {
      for (a <- answer) {
        val reverse = (a._2, a._1)
        if (!(raceList.contains(a) || raceList.contains(reverse))) {
          return false
        }
      }

      return true
    }
}