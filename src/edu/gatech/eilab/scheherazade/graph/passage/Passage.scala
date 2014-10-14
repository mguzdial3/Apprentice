package edu.gatech.eilab.scheherazade

import data._
import main._
import graph._
import scala.collection.mutable.ListBuffer

package graph.passage {

  class Passage(id: Int, graph: Graph, sources: List[Cluster], ends: List[Cluster], val mutex: List[MutualExcl],
    val optional: List[Cluster], history: List[Cluster], fringe: List[Cluster], excluded: List[Cluster]) extends AbstractPassage(
    id, graph, sources, ends, history, fringe, excluded) {

    def this(graph: Graph, sources: List[Cluster], ends: List[Cluster], mutex: List[MutualExcl], optional: List[Cluster], fringe: List[Cluster]) =
      this(0, graph, sources, ends, mutex, optional, List[Cluster](), fringe, List[Cluster]())

    def totalGenerated() = Passage.totalGenerated
      
    /**
     *  Takes one step forward in the graph
     *
     *
     */
    def forward(next: Cluster): Passage =
      {
        val step = next
        if (!(fringe contains step)) throw new RuntimeException("Illegal step: " + step.name)

        // add the step to history. Note history is in reverse order
        val newHistory = step :: history

        // direct mutex
        var newlyExcluded = computeExcluded(List(step), mutex).filter(graph.nodes contains)

        var allExcluded = newlyExcluded ::: excluded

        // recursive mutex
        allExcluded = findTransitiveClosure(graph, allExcluded)

        // update newly excluded. These are used later
        newlyExcluded = allExcluded filterNot (excluded contains)

        // nodes cannot execute any more due to temporal constraints
        val expired = graph.links.filter(l => l.target == step).map(_.source)

        // first add links for skipped nodes and then remove these nodes
        val newGraph = graph.addSkipLinks(newlyExcluded).removeNodes(newlyExcluded ::: expired)

        var newFringe = maxFringe(newGraph, newHistory)
        // delete those already executed
        newFringe = newFringe filterNot (newHistory contains)
        // all steps preceding the step is prohibited

        // the following line enforces an ordering for parallel events, but we now do not need it
        // val parallel = graph.nodes.filterNot(c => graph.ordered(step, c)).filter(c => c.name > step.name)

        if (Passage.debug) {
          println("*******************************")
          println("Walk " + id)
          println("taking step: " + step.name)
          println("excluded because of ME: " + newlyExcluded.map(_.name).mkString("(", ", ", ")"))
          //println("excluded because of symmetry: " + parallel.map(_.name).mkString("(", ", ", ")"))
          println("excluded because of temporal ordering: " + expired.map(_.name).mkString("(", ", ", ")"))
          println("excluded by parent: " + allExcluded.map(_.name).mkString("(", ", ", ")"))

          newGraph.draw("valid")

          println("final fringe: " + newFringe.map(_.name).mkString("(", ", ", ")"))

          println("next story : " + id + "\n*******************************")
          readLine()
        }

        new Passage(Passage.nextId(), newGraph, sources, ends, mutex, optional, newHistory, newFringe, allExcluded)
      }

    /**
     * Can we walk more? Not if the history already contains one story ending
     *
     */
    def hasMoreSteps() = (!fringe.isEmpty) && (!(ends exists { history contains }))

    protected def maxFringe(newGraph: Graph, newHistory: List[Cluster]) =
      {
        // if all of its parents are either included in the history or optionals, it is on the fringe
        val parents = optional ::: newHistory
        newGraph.nodes.filter { node =>
          newGraph.parentsOf(node).forall(parents.contains)
        }
      }

    protected def computeExcluded(history: List[Cluster], melinks: List[MutualExcl]): List[Cluster] =
      {
        history.flatMap(s => melinks.filter(m => m.c1 == s).map(m => m.c2) :::
          melinks.filter(m => m.c2 == s).map(m => m.c1))
      }

    /**
     * if all direct predecessors of an event is in the event list, add that event to the event list
     * continue adding events until no such event exists
     */
    protected def findTransitiveClosure(graph: Graph, events: List[Cluster]): List[Cluster] =
      {

        var all = ListBuffer[Cluster]() ++ events
        var newFound: ListBuffer[Cluster] = null
        var remainder = graph.nodes filterNot (all contains)
        do {
          newFound = ListBuffer[Cluster]()
          for (e <- remainder) {
            val pred = graph.parentsOf(e)
            if ((!pred.isEmpty) &&
              pred.forall(all contains))
              newFound += e
          }
          all ++= newFound
          remainder = remainder filterNot (newFound contains)
        } while (!newFound.isEmpty)

        all.toList
      }

    override def equals(o: Any): Boolean = o match {
      case that: Passage => {
        if (this.history.size != that.history.length) return false
        else {
          val size = this.history.size
          for (i <- 0 until size) {
            if (this.history(i) != that.history(i))
              return false
          }
          return true
        }
      }
      case _ => false
    }

    override def hashCode(): Int = {
      history.map(_.hashCode).sum * 389 / 311
    }
  }

  object Passage extends AbstractPassageFactory {
 
    def init(graph: Graph):Passage = {
      val me = graph.mutualExcls

      // starting point:
      var sources = graph.findSources()
      //println(sources.map(_.name).mkString("sources 1: ", "\n", ""))
      val ends = graph.findEnds()
      //println(ends.map(_.name).mkString("ends : ", "\n", ""))
      //readLine()

      val graphIndex = new GraphIndex(graph)

      // remove from the graph nodes without predecessors that are not sources
      //graph = eGraph.removeIrregularSourceEnds()

      val (optionals, conditionals) = graph.findOptionals()

      val canSkip = (optionals ::: conditionals).distinct
      //println("can Skip : " + canSkip.map(_.name).mkString("\n"))
      val finalGraph = graph.addSkipLinks(canSkip)

      sources = finalGraph.nodes.filter(n => (!sources.contains(n)) &&
        finalGraph.links.filter(l => l.target == n).map(_.source).forall(optionals contains)) ::: sources

      //println(sources.map(_.name).mkString("sources 2 : ", "\n", ""))

      new Passage(finalGraph, sources, ends, me, optionals, sources)
    }
  }
}