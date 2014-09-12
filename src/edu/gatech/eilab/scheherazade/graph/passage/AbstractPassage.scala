package edu.gatech.eilab.scheherazade.graph.passage

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

abstract class AbstractPassage(val id: Int, val graph: Graph, val sources: List[Cluster], val ends: List[Cluster], 
    val history: List[Cluster], val fringe: List[Cluster], val excluded: List[Cluster]) {

  def forward(next: Cluster): AbstractPassage
  def hasMoreSteps():Boolean
  def totalGenerated():Int
}

abstract class AbstractPassageFactory() {
  var debug = false
  
  protected[passage] var count = 0
  
  def totalGenerated() = count
  
  protected[passage] def nextId() = {
    count += 1
    count
  }
  
  def init(graph: Graph): AbstractPassage
}