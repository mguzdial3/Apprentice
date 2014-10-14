package edu.gatech.eilab.scheherazade.graph.sample

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

/** this class contains a number of test cases for CfR (i.e. cause for removal)
 *  
 */
object CRFPropagation {
  
  /** This is Figure 18(b)
   *  
   */
  def graph1() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    
    val nodes = List(a, b, c, d, e, f)

    val links = List(
      new Link(a, b),
      new Link(a, c),
      new Link(b, f),
      new Link(c, f))
      
    val mutex = List(
        new MutualExcl(b, e),
        new MutualExcl(b, d),
        new MutualExcl(c, d)
        )
    
    new Graph(nodes, links, mutex)
  }
  
  /** This is Figure 18(c)
   *  
   */
  def graph2() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    
    val nodes = List(a, b, c, d, e, f)

    val links = List(
      new Link(a, c),
      new Link(b, f),
      new Link(c, f),
      new Link(e, d))
      
    val mutex = List(
        new MutualExcl(b, e),
        new MutualExcl(c, d)
        )
    
    new Graph(nodes, links, mutex)
  }
  
  /** This is Figure 18(d)
   *  
   */
  def graph3() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    val g = new Cluster("g", Nil)
    val h = new Cluster("h", Nil)
    val i = new Cluster("i", Nil)
    
    val nodes = List(a, b, c, d, e, f, g, h, i)

    val links = List(
      new Link(a, i),
      new Link(b, f),
      new Link(c, g),
      new Link(f, i),
      new Link(g, i),
      new Link(d, h),
      new Link(e, h))
      
    val mutex = List(
        new MutualExcl(a, d),
        new MutualExcl(b, e),
        new MutualExcl(g, h)
        )
    
    new Graph(nodes, links, mutex)
  }
  
 /** This is Figure 18(e)
   *  
   */
  def graph4() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    val g = new Cluster("g", Nil)
    val h = new Cluster("h", Nil)
    
    val nodes = List(a, b, c, d, e, f, g, h)

    val links = List(
      new Link(a, c),
      new Link(b, f),
      new Link(c, f),
      new Link(d, e),
      new Link(f, h))
      
    val mutex = List(
        new MutualExcl(b, d),
        new MutualExcl(c, e),
        new MutualExcl(f, g)
        )
    
    new Graph(nodes, links, mutex)
  }
  
  /** This is Figure 18(d) modified
   *  cfr(i) = {{d, e, h}} 
   */
  def graph5() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    val g = new Cluster("g", Nil)
    val h = new Cluster("h", Nil)
    val i = new Cluster("i", Nil)
    val j = new Cluster("j", Nil)
    
    val nodes = List(a, b, c, d, e, f, g, h, i, j)

    val links = List(
      new Link(a, j),
      new Link(j, i),
      new Link(b, f),
      new Link(c, g),
      new Link(f, i),
      new Link(g, i),
      new Link(d, h),
      new Link(e, h))
      
    val mutex = List(
        new MutualExcl(a, d),
        new MutualExcl(b, e),
        new MutualExcl(g, h)
        )
    
    new Graph(nodes, links, mutex)
  }
  
  /** This is Figure 18(d) modified
   *  cfr(i) = {{d, e, h}, {d, l, h}} 
   */
  def graph6() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    val g = new Cluster("g", Nil)
    val h = new Cluster("h", Nil)
    val i = new Cluster("i", Nil)
    val j = new Cluster("j", Nil)
    val l = new Cluster("l", Nil)
    
    val nodes = List(a, b, c, d, e, f, g, h, i, j, l)

    val links = List(
      new Link(a, j),
      new Link(l, h),
      new Link(j, i),
      new Link(b, f),
      new Link(c, g),
      new Link(f, i),
      new Link(g, i),
      new Link(d, h),
      new Link(e, h))
      
    val mutex = List(
        new MutualExcl(a, d),
        new MutualExcl(b, e),
        new MutualExcl(b, l),
        new MutualExcl(g, h)
        )
    
    new Graph(nodes, links, mutex)
  }
}