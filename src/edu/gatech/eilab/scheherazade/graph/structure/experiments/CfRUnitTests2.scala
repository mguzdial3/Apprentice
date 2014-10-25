package edu.gatech.eilab.scheherazade.graph.structure.experiments

import org.scalatest.FunSuite
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.graph.sample._

/**
 * Created Oct 23 2014
 *
 */
class CfRUnitTests2 extends FunSuite {
  test("Sample Graph 1") {
    val graph = CfRSample.graph1
    val answer = CfRComputer2.processGraph(graph)
    val result = answer._1
    println(answer._1)
    println(answer._2)
    val f = result(new Cluster("f", Nil))
    assert(f.size == 1 && f.contains(new CauseForRemoval(new Cluster("d", Nil), Nil)))
    val raceCond = answer._2
    assert(raceCond.size == 1 && raceCond.contains(new RaceCondition(new Cluster("f", Nil), List(new Cluster("d", Nil)), List(new Cluster("e", Nil)))))
    println("race conditions = " + raceCond)
  }

  test("Sample Graph 2") {
    val graph = CfRSample.graph2
    val result = CfRComputer2.processGraph(graph)._1
    val f = result(new Cluster("f", Nil))
    assert(f.size == 1 && f.contains(new CauseForRemoval(new Cluster("d", Nil), List(new Cluster("e", Nil)))))
  }

  test("Sample Graph 3") {
    val graph = CfRSample.graph3
    val result = CfRComputer2.processGraph(graph)._1
    val i = result(new Cluster("i", Nil))
    assert(i.size == 1 && i.contains(new CauseForRemoval(new Cluster("h", Nil), List(new Cluster("d", Nil), new Cluster("e", Nil)))))
  }

  test("Sample Graph 4") {
    val graph = CfRSample.graph4
    val result = CfRComputer2.processGraph(graph)._1
    val f = result(new Cluster("h", Nil))
    assert(f.size == 2 && f.contains(new CauseForRemoval(new Cluster("e", Nil), List(new Cluster("d", Nil)))) &&
      f.contains(new CauseForRemoval(new Cluster("g", Nil), Nil)))
  }

  test("Sample Graph 5") {
    val graph = CfRSample.graph5
    val result = CfRComputer2.processGraph(graph)._1
    val i = result(new Cluster("i", Nil))

    assert(i.size == 1 && i.contains(new CauseForRemoval(new Cluster("h", Nil), List(new Cluster("d", Nil), new Cluster("e", Nil)))))
  }

  test("Sample Graph 6") {
    val graph = CfRSample.graph6
    val result = CfRComputer2.processGraph(graph)._1
    val i = result(new Cluster("i", Nil))
    println(i)
    assert(i.size == 2 && i.contains(new CauseForRemoval(new Cluster("h", Nil), List(new Cluster("d", Nil), new Cluster("e", Nil)))) &&
      i.contains(new CauseForRemoval(new Cluster("h", Nil), List(new Cluster("d", Nil), new Cluster("l", Nil)))))
  }

  test("Sample Graph 7") {
    val graph = CfRSample.graph7
    val answer = CfRComputer2.processGraph(graph)
    val cfr = answer._1
    val f = cfr(new Cluster("i", Nil))
    assert(f.size == 1 && f.contains(new CauseForRemoval(new Cluster("h", Nil), List(new Cluster("d", Nil), new Cluster("l", Nil)))))
    val raceCond = answer._2
    println(raceCond)
    assert(raceCond.contains(new RaceCondition((new Cluster("i", Nil)), List(new Cluster("d", Nil), new Cluster("e", Nil)), List(new Cluster("h", Nil)))))
  }

  test("Sample Graph 8") {
    val cfr1 = new CauseForRemoval(new Cluster("a", Nil), List(new Cluster("b", Nil)))
    val cfr2 = new CauseForRemoval(new Cluster("a", Nil), Nil)
    assert(cfr1.strictSuperSetOf(cfr2))

    val graph = CfRSample.graph8
    val result = CfRComputer2.processGraph(graph)._1
    val f = result(new Cluster("g", Nil))

    assert(f.size == 1 && f.contains(new CauseForRemoval(new Cluster("d", Nil), Nil)))
  }

  test("Sample Graph 9") {
    val graph = CfRSample.graph9
    val result = CfRComputer2.processGraph(graph)._1
    val f = result(new Cluster("e", Nil))
    assert(f.size == 1 && f.contains(new CauseForRemoval(null, List(new Cluster("c", Nil)))))
  }

  test("Sample Graph 10") {
    val graph = CfRSample.graph10
    val result = CfRComputer2.processGraph(graph)._1
    val f = result(new Cluster("f", Nil))
    assert(f.size == 0)
  }

  test("Sample Graph 11") {
    val graph = CfRSample.graph11
    //graph.draw("aaaa")
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    println(CfRComputer2.formatMap(map))
    val d = map(new Cluster("d", Nil))
    assert(d.size == 2 && d.contains(new CauseForRemoval(new Cluster("g", Nil), Nil)) && d.contains(new CauseForRemoval(new Cluster("f", Nil), Nil)))
    val e = map(new Cluster("e", Nil))
    assert(e.size == 1 && e.contains(new CauseForRemoval(new Cluster("g", Nil), Nil)))
    val raceCond = answer._2
    println(raceCond)
    assert(raceCond.size == 2 && raceCond.contains(new RaceCondition(new Cluster("e", Nil), List(new Cluster("f", Nil)), List(new Cluster("g", Nil)))) &&
      raceCond.contains(new RaceCondition(new Cluster("h", Nil), List(new Cluster("d", Nil)), List(new Cluster("b", Nil)))))
  }

  test("Sample Graph 12") {
    val graph = CfRSample.graph12
    //graph.draw("aaaa")
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    //println(map)
    assert(map(new Cluster("i", Nil)) == Nil)
    val raceCond = answer._2
    assert(raceCond == Nil)
    println("races = " + raceCond)
  }

  test("Sample Graph 13") {
    val graph = CfRSample.graph13
    //graph.draw("aaaa")
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    println("cfr = " + CfRComputer2.formatMap(map))
    val raceCond = answer._2
    assert(raceCond.contains(new RaceCondition(new Cluster("j", Nil), List(new Cluster("h", Nil)), List(new Cluster("g", Nil), new Cluster("f", Nil)))))
    //println("races = " + raceCond)
  }

  test("Sample Graph 14") {
    val graph = CfRSample.graph14
    //graph.draw("aaaa")
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    println("cfr = " + CfRComputer2.formatMap(map))
    val j = new Cluster("j", Nil)
    val jcfr = map(j)
    assert(jcfr.size == 1 && jcfr.contains(new CauseForRemoval(new Cluster("h", Nil), List(new Cluster("d", Nil), new Cluster("e", Nil)))))
    val raceCond = answer._2
    assert(raceCond == Nil)
  }

  test("Sample Graph 15") {
    val graph = CfRSample.graph15
    //graph.draw("aaaa")
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    println("cfr = " + CfRComputer2.formatMap(map))
    val j = new Cluster("j", Nil)
    val jcfr = map(j)
    assert(jcfr.size == 0)
    val raceCond = answer._2
    assert(raceCond == Nil)
  }

  test("Sample Graph 16") {
    val graph = CfRSample.graph16
    //graph.draw("aaaa")
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    println("cfr = " + CfRComputer2.formatMap(map))
    val j = new Cluster("j", Nil)
    val jcfr = map(j)
    assert(jcfr.size == 1 && jcfr.contains(new CauseForRemoval(new Cluster("m", Nil), Nil)))
    val raceCond = answer._2
    assert(raceCond.size == 1 && raceCond.contains(new RaceCondition(new Cluster("j", Nil), List(new Cluster("m", Nil)), List(new Cluster("h", Nil), new Cluster("e", Nil), new Cluster("d", Nil)))))
  }

  test("Sample Graph 17") {
    val graph = CfRSample.graph17
    //graph.draw("aaaa")
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    val raceCond = answer._2
    println("cfr = " + CfRComputer2.formatMap(map))
    println(raceCond)
    val i = new Cluster("i", Nil)
    val icfr = map(i)
    assert(icfr.size == 1 && icfr.contains(new CauseForRemoval(new Cluster("e", Nil), List(new Cluster("d", Nil)))))
    assert(raceCond.size == 2 && raceCond.contains(new RaceCondition(new Cluster("i", Nil), List(new Cluster("h", Nil)), List(new Cluster("e", Nil)))) &&
      raceCond.contains(new RaceCondition(new Cluster("i", Nil), List(new Cluster("h", Nil)), List(new Cluster("d", Nil), new Cluster("e", Nil)))))
  }

  test("Sample Graph 18") {
    val graph = CfRSample.graph18
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    val raceCond = answer._2
    val i = new Cluster("i", Nil)
    assert(map(new Cluster("i", Nil)).isEmpty)
    assert(map(new Cluster("d", Nil)).contains(new CauseForRemoval(new Cluster("b", Nil), Nil)))
    assert(map(new Cluster("c", Nil)).contains(new CauseForRemoval(null, List(new Cluster("b", Nil)))))
    assert(raceCond.isEmpty)
  }

  test("Sample Graph 19") {
    val graph = CfRSample.graph19
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    val raceCond = answer._2
    val condPrec = answer._3
    assert(raceCond.contains(new RaceCondition(new Cluster("e", Nil), List(new Cluster("c", Nil)), List(new Cluster("d", Nil)))))
  }

  test("Sample Graph 20") {
    val graph = CfRSample.graph20
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    val raceCond = answer._2
    val condPrec = answer._3
    println(condPrec)
  }

  test("Sample Graph 21") {
    val graph = CfRSample.graph21
    val answer = CfRComputer2.processGraph(graph)
    val map = answer._1
    val raceCond = answer._2
    val condPrec = answer._3
    assert(condPrec.size == 3 && condPrec.contains(new ConditionalPrec(List(new Cluster("c", Nil), new Cluster("d", Nil)), new Cluster("f", Nil)))
      && condPrec.contains(new ConditionalPrec(List(new Cluster("d", Nil)), new Cluster("e", Nil)))
      && condPrec.contains(new ConditionalPrec(List(new Cluster("c", Nil)), new Cluster("e", Nil))))
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