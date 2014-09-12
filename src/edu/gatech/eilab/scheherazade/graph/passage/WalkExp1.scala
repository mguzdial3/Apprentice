package edu.gatech.eilab.scheherazade

import data._
import graph._
import analysis._
import io._
import scala.collection.mutable.ListBuffer
import java.io._

package graph.passage {

  object PassageExp1 extends AbstractPassageFactory {

    def init(graph: Graph) = {

      val MI_THRESHOLD = 0.05

      // generate graph
      val reader = new ConfigReader("configRobBest.txt")
      var (stories, clusters) = reader.initDataFiltered()

      val para = reader.properties.allParameters()(0)

      val minimumSize = para.intParam("minClusterSize")
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      val gen = new GraphGenerator(insideStories, insideClusters)
      var graph: Graph = gen.generateQIP(para)("mutualExcl")._1
      graph.draw("base-graph")
      Thread.sleep(1000)

      val me = graph.mutualExcls
      println("me: " + me.mkString("\n"))
      // starting point:
      var sources = graph.nodes.filterNot(n => graph.links.exists(l => l.target == n))
      //println(sources.map(_.name).mkString("sources : ", "\n", ""))
      val ends = graph.nodes.filterNot(n => graph.links.exists(l => l.source == n))
      //println(ends.map(_.name).mkString("ends : ", "\n", ""))
      //readLine()

      // remove from the graph nodes without predecessors that are not sources
      //graph = eGraph.removeIrregularSourceEnds()

      graph = graph.graphWithOptionalsAndSkips // add links that skip over optionals

      sources = graph.nodes.filter {
        n =>
          (!sources.contains(n)) &&
            graph.links.filter(l => l.target == n).map(_.source).forall(graph.optionals.contains)
      } ::: sources
      // after adding the skip links, the sources also include nodes whose parents are all optionals.  

      val firstWalk = new PassageExp1(nextId(), graph, sources, ends, Nil, Nil, Nil)
      firstWalk

    }
  }

  class PassageExp1(id: Int, graph: Graph, sources: List[Cluster], ends: List[Cluster], history: List[Cluster],
    fringe: List[Cluster], excluded: List[Cluster]) extends AbstractPassage(
    id, graph, sources, ends, history, fringe, excluded) {

    def this(id: Int, graph: Graph, sources: List[Cluster], ends: List[Cluster], fringe: List[Cluster]) = this(id, graph, sources,
      ends, List[Cluster](), fringe, List[Cluster]())

    def totalGenerated() = Passage.totalGenerated
      
    /**
     * Takes one step in the graph
     *
     */
    def forward(step: Cluster): PassageExp1 = {
      val newHistory = step :: history
      var newlyExcluded = findExcluded(List(step), graph.mutualExcls).filter(graph.nodes.contains)
      //println(excluded.map(_.name).mkString("directly mutex: ", ", ", ""))
      //println(exclList.map(_.name).mkString("old mutex: ", ", ", ""))
      var excl = newlyExcluded ::: excluded
      excl = findTransitiveClosure(graph, excl)
      //println(excl.map(_.name).mkString("closure mutex: ", ", ", ""))
      
      val expired = graph.links.filter(l => l.target == step).map(_.source).filterNot(newHistory.contains) // preventing deleting history
      var newGraph = graph.detectAndAddSkipLinks(excluded).removeNodes(excluded ::: expired)

      newGraph = new Graph(newGraph.nodes, newGraph.links.distinct, newGraph.mutualExcls, newGraph.optionals, newGraph.conditionals)
      var newFringe = findFringe(newHistory, newGraph)
      // delete those already executed
      newFringe = newFringe filterNot (newHistory contains)
      // all steps preceding the step is prohibited

      if (PassageExp1.debug) {
        newlyExcluded = excl filterNot (excluded.contains) // update this for debugging purpose
        
        println("*******************************")
        println(this)
        println("taking step: " + step.name)
        println("excluded because of ME: " + newlyExcluded.map(_.name).mkString("(", ", ", ")"))
        println("excluded because of temporal ordering: " + expired.map(_.name).mkString("(", ", ", ")"))
        println("excluded by parent: " + excluded.map(_.name).mkString("(", ", ", ")"))
        println("is temporal ordering removal necessary: " + (newFringe filter (expired contains)).map(_.name).mkString)

        newGraph.draw("valid")

      }

      /*
        // exclude symmetry
        newFringe =
          if ((newFringe -- parallel).isEmpty) newFringe
          else (newFringe -- parallel)
		*/

      val id = PassageExp1.nextId()
      if (PassageExp1.debug) {
        println("final fringe: " + newFringe.map(_.name).mkString("(", ", ", ")"))

        println("next story : " + id + "\n*******************************")
        readLine()
      }
      
      new PassageExp1(id, newGraph, this.sources, this.ends, newHistory, newFringe, excl)
    }

    /**
     * return all possible steps
     *
     */
    def possibleSteps(): List[PassageExp1] =
      {
        fringe map { step =>
          forward(step)
        }
      }

    /**
     * This is a specific function different from other "transitive closure". This function ignores links whose kind field is not equal to "R"
     */
    def findTransitiveClosure(graph: Graph, events: List[Cluster]): List[Cluster] =
      {

        //        val canSkip = events.filterNot(e => graph.optionals.contains(e) || graph.conditionals.contains(e)) // optional or conditionals do not apply
        //        val cannotSkip = events.filterNot(canSkip contains)

        var all = ListBuffer[Cluster]() ++ events
        var newFound: ListBuffer[Cluster] = null
        var remainder = graph.nodes filterNot (all.contains)
        do {
          newFound = ListBuffer[Cluster]()
          for (e <- remainder) {

            val pred = graph.links.filter(l => l.kind == "R" && l.target == e).map(_.source)

            if ((!pred.isEmpty) && pred.forall(all.contains)) {
              newFound += e
            }
          }
          all ++= newFound
          remainder = remainder filterNot (newFound.contains)
        } while (!newFound.isEmpty)

        all.toList //::: canSkip
      }

    def hasMoreSteps() = !(fringe.isEmpty) && (!ends.exists(history.contains))

    override def toString(): String = {
      history.reverse.map(_.name).mkString("Story: " + id + "\n", "\n", "\n***\n")
    }

    override def equals(o: Any): Boolean = o match {
      case that: PassageExp1 => {
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

    private def findFringe(history:List[Cluster], graph:Graph) =
      {
        // if all of its parents are either included in the history or optionals, it is on the fringe
        val parents = graph.optionals ::: history
        var possible = graph.nodes.filter { node =>
          graph.predecessorsOf(node).forall(parents.contains)
        }
        possible
      }

    private def findExcluded(history: List[Cluster], melinks: List[MutualExcl]): List[Cluster] =
      {
        history.flatMap(s => melinks.filter(m => m.c1 == s).map(m => m.c2) :::
          melinks.filter(m => m.c2 == s).map(m => m.c1))
      }

  }

  //  object WalkOnDisk {
  //
  //    var id = 0
  //
  //    private def nextId() = {
  //      id += 1
  //      id
  //    }

  //
  //    /**
  //     * nodes excluded with mutual exclusion links
  //     *
  //     */

  //
  //    def fromInits(inits: List[Cluster], graph: Graph, debug: Boolean = false): WalkOnDisk = {
  //      val fringe = inits
  //      //      val realEnds = graph.nodes.filter(n => graph.links.exists(l => l.source == n && l.target.name == "END"))
  //      //      println("real ends " + realEnds)
  //      
  //    }
  //  }

}
