package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

/**
 * requires that if everything in the before list are executed, then the after cluster must happen later than those in the before list
 *
 */
class ConditionalPrec(val before: List[Cluster], val after: Cluster) {

  override def equals(o: Any): Boolean =
    {
      o match {
        case that: ConditionalPrec =>
          this.after == that.after && this.before.filterNot(that.before.contains) == Nil && that.before.filterNot(this.before.contains) == Nil
        case _ => false
      }
    }

  override def hashCode() =
    {
      var sum = 0
      for (v <- before) {
        sum += v.hashCode
      }
      sum += after.hashCode
      sum * 97 / 31
    }

  override def toString() =
    {
      val buf = new StringBuffer()
      buf.append("[Prec: ")
      buf.append(before.map(_.name).mkString(","))
      buf.append(" < ")
      buf.append(after.name)
      buf.append("]")
      buf.toString
    }

}