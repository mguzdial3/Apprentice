package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

class ForcedCooccurence(val precursor: Cluster, val dependant: Cluster) {

  override def equals(o: Any): Boolean =
    o match {
      case that: ForcedCooccurence =>
        this.precursor == that.precursor && that.dependant == this.dependant
    }

  override def hashCode() =
    {
      var sum = 0
      sum += precursor.hashCode
      sum += dependant.hashCode
      sum * 107 / 53
    }

  override def toString() =
    {
      val buf = new StringBuffer()
      buf append "[FC ("
      buf append (precursor.name)
      buf append "->"
      buf append (dependant.name)
      buf append ")]"
      buf toString
    }

}