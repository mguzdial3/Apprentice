package edu.gatech.eilab.scheherazade.graph.structure.experiments

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

final class RaceCondition (val a:List[Cluster], val b:List[Cluster]){
	
  override def equals(o:Any):Boolean =
  {
    o match {
      case rc:RaceCondition => 
        (setEqual(this.a, rc.a) && setEqual(this.b, rc.b)) ||
        (setEqual(this.b, rc.a) && setEqual(this.a, rc.b))
      case _ => false
    }
  }
  
  override def hashCode() =
  {
    var sum = 0
    for (ac <- a) sum += a.hashCode()
    for (bc <- b) sum += b.hashCode()
    
    sum * 1721 / 271
  }
  
  override def toString():String =
  {
    var buffer = new StringBuffer()
    buffer.append(a.map(_.name).mkString("(", ", ", ")"))
    buffer.append(" -/- ")
    buffer.append(b.map(_.name).mkString("(", ", ", ")"))
    buffer.toString()
  }
  
  def setEqual(a:List[Cluster], b:List[Cluster]):Boolean =
  {
    a.filterNot(b.contains) == Nil && b.filterNot(a.contains) == Nil
  }
}