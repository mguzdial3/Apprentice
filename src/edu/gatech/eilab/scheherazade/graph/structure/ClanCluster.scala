package edu.gatech.eilab.scheherazade.graph.structure
import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

class ClanCluster(name: String, override val members: List[Sentence], val clusters: List[Cluster], val links: List[Link]) extends Cluster(name, members)

object ClanCluster {
  def apply(eg: EventGroup, graphLinks:List[Link]): ClanCluster = {
	require(eg.isValid) // must be a valid event group
	
	val nodes = eg.nodes
	val name = nodes.map(_.name).mkString("+")
	val internalLinks = graphLinks.filter(l => nodes.contains(l.source) && nodes.contains(l.target))
	val members = nodes.flatMap(_.members)
	
	new ClanCluster(name, members, nodes, internalLinks)
  }
}