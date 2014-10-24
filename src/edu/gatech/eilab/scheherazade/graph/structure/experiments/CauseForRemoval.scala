package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

/**
 * we need to note the specialty of group 3 vertices.
 *  In any CauseForRemoval, there can only be zero or one group 3 vertices. If it contains more than one group 3 vertices, or if the group 3 can happen earlier
 *  than any other vertices, this is not a valid CfR.
 *  If the group 3 must happen earlier, it is neither a CfR nor a race condition.
 *  If it may or may not happen earlier, it is a race condition
 *
 *  group3 could be null, when no such vertex is required.
 */
final class CauseForRemoval(val group3: Cluster, val other: List[Cluster], val raceCondition: Boolean = false) {

  def allVertices: List[Cluster] =
    {
      if (group3 != null) {
        group3 :: other
      } else {
        other
      }
    }

  override def equals(o: Any): Boolean =
    o match {
      case that: CauseForRemoval =>
        this.group3 == that.group3 && this.other.filterNot(that.other.contains) == Nil && that.other.filterNot(this.other.contains) == Nil &&
        this.raceCondition == that.raceCondition
      case _ => false
    }

  override def hashCode() =
    (group3.hashCode * 97 + other.hashCode * 109 + {if (raceCondition) 1 else 0} ) % 61

  override def toString() =
    {
      val buf = new StringBuffer()
      buf.append("[CfR ")
      if (group3 == null) {
        buf.append("nil")
      } else {
        buf.append(group3.name)
      }
      buf.append("|")
      buf.append(other.map(_.name).mkString(","))
      if (raceCondition)
      {
        buf.append("|R")
      }
      buf.append("]")
      buf.toString
    }

  def checkMx(graph: Graph, that: CauseForRemoval): Boolean =
    {
      for (a <- this.other; b <- that.other) {
        if (graph.mutuallyExclusive(a, b)) {
          return false
        }
      }

      if (this.group3 != null) {
        for (b <- that.other) {
          if (graph.mutuallyExclusive(this.group3, b))
            return false
        }
      }

      if (that.group3 != null) {
        for (a <- this.other) {
          if (graph.mutuallyExclusive(that.group3, a)) {
            return false
          }
        }
      }
      return true
    }

  /**
   * returns 0, 1, or 2
   *  0: compatible, can merge
   *  1: potentially race condition
   *  2: impossible to work together as a cfr.
   */
  def compatibleWith(graph: Graph, that: CauseForRemoval): Int = {
    val group3IsGood = this.group3 == null || that.group3 == null || this.group3 == that.group3

    if (!group3IsGood) return CauseForRemoval.IMPOSSIBLE

    /** check for mutual exclusions **/
    val mutexCheckPass = checkMx(graph, that)
    if (!mutexCheckPass) return CauseForRemoval.IMPOSSIBLE

    val realGroup3 = {
      if (this.group3 == null) that.group3
      else this.group3
    }

    if (realGroup3 != null) {
      val all = this.other ::: that.other
      val correctlyOrdered = all.forall(x => graph.shortestDistance(x, realGroup3) >= 0)
      val impossible = all.exists(x => graph.shortestDistance(realGroup3, x) >= 0)

      if (correctlyOrdered) return CauseForRemoval.COMPATIBLE
      else if (impossible) return CauseForRemoval.IMPOSSIBLE
      else return CauseForRemoval.RACE_CONDITION
    }

    return CauseForRemoval.COMPATIBLE

  }

  def mergeWith(that: CauseForRemoval): CauseForRemoval =
    {
      val realGroup3 = {
        if (this.group3 == null) that.group3
        else this.group3
      }

      new CauseForRemoval(realGroup3, (this.other ::: that.other).distinct)
    }

  def mergeRaceCondition(that: CauseForRemoval): CauseForRemoval =
    {
      val realGroup3 = {
        if (this.group3 == null) that.group3
        else this.group3
      }

      new CauseForRemoval(realGroup3, this.other ::: that.other, true)
    }

}

object CauseForRemoval {
  val COMPATIBLE = 0
  val RACE_CONDITION = 1
  val IMPOSSIBLE = 2
}