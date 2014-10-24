package edu.gatech.eilab.scheherazade.graph.sample

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.graph._

/**
 * this class contains a number of test cases for CfR (i.e. cause for removal)
 *
 */
object CfRSample {

  /**
   * This is Figure 18(b) and also 21(a)
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
      new MutualExcl(c, d))

    new Graph(nodes, links, mutex)
  }

  /**
   * This is Figure 18(c)
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
      new MutualExcl(c, d))

    new Graph(nodes, links, mutex)
  }

  /**
   * This is Figure 18(d)
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
      new MutualExcl(g, h))

    new Graph(nodes, links, mutex)
  }

  /**
   * This is Figure 18(e)
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
      new MutualExcl(f, g))

    new Graph(nodes, links, mutex)
  }

  /**
   * This is Figure 18(d) modified
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
      new MutualExcl(g, h))

    new Graph(nodes, links, mutex)
  }

  /**
   * This is graph5 modified. A new vertex l is added as the parent of h.
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
      new MutualExcl(g, h))

    new Graph(nodes, links, mutex)
  }

  /**
   * This is graph 6 modified. It violates the ordering check.
   *  cfr(i) = { {d, l, h} }
   *  Race condition = h-/-(e,d). There is an extra race condition e-/-dhl, which is not necesssary. should be removed in the future.
   */
  def graph7() = {
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
      new Link(d, h))

    val mutex = List(
      new MutualExcl(a, d),
      new MutualExcl(b, e),
      new MutualExcl(b, l),
      new MutualExcl(g, h))

    new Graph(nodes, links, mutex)
  }

  /**
   * This is Figure 19(a). It tests the ability to simplify CfR boolean formulas
   *  cfr(g) = { {d}, {c, d} } = {{d}}
   */
  def graph8() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    val g = new Cluster("g", Nil)

    val nodes = List(a, b, c, d, e, f, g)

    val links = List(
      new Link(a, e),
      new Link(b, e),
      new Link(b, f),
      new Link(e, g),
      new Link(f, g),
      new Link(c, d))

    val mutex = List(
      new MutualExcl(b, c),
      new MutualExcl(d, e),
      new MutualExcl(d, f))

    new Graph(nodes, links, mutex)
  }

  /**
   * This is figure 19(b)
   *  cfr(e) = { {c,c,c} } = {{c}}
   */
  def graph9() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    val g = new Cluster("g", Nil)

    val nodes = List(a, b, c, d, e, f, g)

    val links = List(
      new Link(a, e),
      new Link(b, d),
      new Link(d, e),
      new Link(e, f),
      new Link(f, g))

    val mutex = List(
      new MutualExcl(a, c),
      new MutualExcl(b, c),
      new MutualExcl(d, g))

    new Graph(nodes, links, mutex)
  }

  /**
   *  This is a graph deleted from the dissertation because it is no longer a race condition.
   *  cfr(f) = { }. f cannot be removed!
   */
  def graph10() = {
    val a = new Cluster("a", Nil)
    val b = new Cluster("b", Nil)
    val c = new Cluster("c", Nil)
    val d = new Cluster("d", Nil)
    val e = new Cluster("e", Nil)
    val f = new Cluster("f", Nil)
    val g = new Cluster("g", Nil)

    val nodes = List(a, b, c, d, e, f, g)

    val links = List(
      new Link(a, b),
      new Link(a, c),
      new Link(a, d),
      new Link(c, e),
      new Link(e, f),
      new Link(b, f),
      new Link(e, g))

    val mutex = List(
      new MutualExcl(b, c),
      new MutualExcl(e, d),
      new MutualExcl(c, g))

    new Graph(nodes, links, mutex)
  }

  /**
   *  This is a race condition graph. G and F are in the race condition. G is mutually exclusive
   *  to both parents of E. F is mutually exclusive to one parent of E. If F executes first, E will
   *  remain in the graph.
   */
  def graph11() = {
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
      new Link(a, b),
      new Link(a, c),
      new Link(c, d),
      new Link(d, e),
      new Link(b, e),
      new Link(g, h),
      new Link(f, h))

    val mutex = List(
      new MutualExcl(f, c),
      new MutualExcl(g, b),
      new MutualExcl(g, d))

    new Graph(nodes, links, mutex)
  }
  
 /**
   * This is another modification to Figure 18(d)
   *
   */
  def graph12() = {
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
      new Link(h, d),
      new Link(h, e))

    val mutex = List(
      new MutualExcl(a, d),
      new MutualExcl(b, e),
      new MutualExcl(g, h))

    new Graph(nodes, links, mutex)
  }
  
  /**
   *  This is a complex race condition graph. 
   *  Race condition between h and g, f
   */
  def graph13() = {
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
    val k = new Cluster("k", Nil)
    val l = new Cluster("l", Nil)
    val m = new Cluster("m", Nil)

    val nodes = List(a, b, c, d, e, f, g, h, i, j, k, l, m)

    val links = List(
      new Link(a, j),
      new Link(b, d),
      new Link(c, e),
      new Link(d, j),
      new Link(e, j),
      new Link(f, g),
      new Link(f, h),
      //new Link(i, j),
      new Link(i, k),
      new Link(k, m),
      new Link(j, m)      
      )

    val mutex = List(
      new MutualExcl(a, f),
      new MutualExcl(g, b),
      new MutualExcl(e, h),
      new MutualExcl(l, j),
      new MutualExcl(l, k)
      )

    new Graph(nodes, links, mutex)
  }
  
   /**
   * This is another modification to Figure 18(d)
   *
   */
  def graph14() = {
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
    val k = new Cluster("k", Nil)
    val l = new Cluster("l", Nil)
    
    val nodes = List(a, b, c, d, e, f, g, h, i, j, k, l)

    val links = List(
      new Link(a, i),
      new Link(b, f),
      new Link(c, g),
      new Link(f, i),
      new Link(g, i),
      new Link(d, h),
      new Link(e, h),
      new Link(l, k),
      new Link(k, j),
      new Link(i, j)
      )

    val mutex = List(
      new MutualExcl(a, d),
      new MutualExcl(b, e),
      new MutualExcl(g, h),
      new MutualExcl(h, k))

    new Graph(nodes, links, mutex)
  }
}