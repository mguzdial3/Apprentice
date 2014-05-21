package edu.gatech.eilab.scheherazade.graph

import edu.gatech.eilab.scheherazade.data._

class ObservedLink(source: Cluster, target: Cluster, val trueInstance: Int, val totalObservations: Int) extends Link(source, target)
{

  override def equals(o: Any) = o match {
    case that: ObservedLink => this.source == that.source && this.target == that.target
    case _ => false
  }

  override def hashCode(): Int = (source.hashCode * 17 + target.hashCode * 23) / 41

  val prob: Double = {
    if (totalObservations == 0)
      0
    else trueInstance.toDouble / totalObservations
  }

  override def toString() = "Link(" + source.name + " -> " + target.name + ", B(" + trueInstance + "/" + totalObservations + ")=" + confidence + ")"

  def addEvidence(positive: Int, negative: Int) = ObservedLink(source, target, trueInstance + positive, totalObservations + positive + negative)

  def confidence = 1 - oneTail(trueInstance, totalObservations, 0.5)

  private def oneTail(k: Int, n: Int, p: Double): Double =
    (k to n) map (binomial(_, n, p)) sum

  private def binomial(k: Int, n: Int, p: Double): Double =
    choose(n, k).toDouble * math.pow(p, k) * math.pow(p, n - k)

  private def choose(n: Int, k: Int): BigInt = (k + 1 to n).foldLeft(BigInt(1))(_ * BigInt(_)) / (1 to n - k).foldLeft(BigInt(1))(_ * BigInt(_))
}

object ObservedLink {

  def apply(source: Cluster, target: Cluster, trueInstance: Int, totalObservations: Int) = new ObservedLink(source, target,
    trueInstance, totalObservations)
}

/**
 * the Link class includes both temporal and causal links
 * the default is temporal if the kind field is not specified
 */
//class Link(val source: Cluster, val target: Cluster, var kind: String = "T") {
//  require(kind == "T" || kind == "C")
//
//  override def toString() = source.name + " -> " + target.name
//  
//  //def graphvisString = source.name.replace(" ", "_") + " -> " + target.name.replace(" ", "_")
//  
//  override def equals(o: Any) = o match {
//    case other: Link => this.source == other.source && this.target == other.target && this.kind == other.kind
//    case _ => false
//  }
//  override def hashCode(): Int = (source.hashCode() + target.hashCode() + kind.hashCode()) * 19 / 97
//  def isTemporal = (kind == "T")
//  def isCausal = (kind == "C")
//}

/**
 * the Link class now does not differentiate between causal and temporal links
 * May 21 2014: added the kind field. If it is not "R", it is not counted when determining if a node can be deleted. 
 * That is, links added for temporal purposes only will have a different value than "R"  
 */
class Link(val source: Cluster, val target: Cluster, val kind:String = "R") {  

  override def toString() = source.name + " -> " + target.name
  
  //def graphvisString = source.name.replace(" ", "_") + " -> " + target.name.replace(" ", "_")
  
  override def equals(o: Any) = o match {
    case other: Link => this.source == other.source && this.target == other.target
    case _ => false
  }
  override def hashCode(): Int = (source.hashCode() + target.hashCode()) * 19 / 97  
}

class MutualExcl(val c1: Cluster, val c2: Cluster) {
  override def toString() = "ME: " + c1.name + " -/- " + c2.name
  
    override def equals(o: Any) = o match {
    case other: MutualExcl => (this.c1 == other.c1 && this.c2 == other.c2) || (this.c2 == other.c1 && this.c1 == other.c2) 
    case _ => false
  }
}