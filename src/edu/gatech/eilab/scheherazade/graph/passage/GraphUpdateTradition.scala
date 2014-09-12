package edu.gatech.eilab.scheherazade.graph.passage

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

object GraphUpdateTradition {

  /**
   * First detects if a skip link is needed. Adds it to the graph if it is needed.
   * A skip link is not needed iff the nodes being skipped is optional or conditional. We could simply check that.
   * However, for the sake of safety, we do the extra detection.
   *
   */
  def detectAndAddSkipLinks(graph: Graph, skipped: List[Cluster]): Graph =
    {
      val removedGraph = graph.removeNodes(skipped) // a graph where the skipped nodes are directly removed without adding skipping links
      //removedGraph.draw("removedgraph")
      var newLinks = graph.links

      for (e <- skipped) {

        // only add a regular link when both links are regular
        // otherwise, add a temporal link   
        // Albert Jun 10 2014
        val regularPredecessors = newLinks.filter(l => l.target == e && l.kind == "R").map(_.source)
        val regularSuccessors = newLinks.filter(l => l.source == e && l.kind == "R").map(_.target)

        val temporalPredecessors = newLinks.filter(l => l.target == e && l.kind == "T").map(_.source)
        val temporalSuccessors = newLinks.filter(l => l.source == e && l.kind == "T").map(_.target)

        // in contrast to GraphUpdateNew.detectAndAddSkipLinks, here we do not check if there is already a link from p to s. 
        // We just add a link from p to s no matter what
        for (p <- regularPredecessors; s <- regularSuccessors) {
            newLinks = new Link(p, s) :: newLinks
        }
        for (p <- temporalPredecessors; s <- regularSuccessors) {          
            newLinks = new Link(p, s, "T") :: newLinks           
        }
        for (p <- regularPredecessors; s <- temporalSuccessors) {          
            newLinks = new Link(p, s, "T") :: newLinks          
        }
        for (p <- temporalPredecessors; s <- temporalSuccessors) {          
            newLinks = new Link(p, s, "T") :: newLinks          
        }
      }

      val g = new Graph(removedGraph.nodes, newLinks, removedGraph.mutualExcls, removedGraph.optionals, removedGraph.conditionals)

      g.addSkipLinks(removedGraph.optionals)
    }
}