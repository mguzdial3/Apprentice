package edu.gatech.eilab.scheherazade

import data._
import java.io._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import edu.gatech.eilab.scheherazade.data.serialize.XStreamable

package graph {
  class Graph(val nodes: List[Cluster], val links: List[Link], val mutualExcls: List[MutualExcl], val optionals: List[Cluster], val conditionals: List[Cluster]) extends XStreamable[Graph] {

    //var optionals: List[Cluster] = Nil

    def this(nodes: List[Cluster], links: List[Link]) = this(nodes, links, List[MutualExcl](), Nil, Nil)
    def this(nodes: List[Cluster], links: List[Link], mutualExcls: List[MutualExcl]) = this(nodes, links, mutualExcls, Nil, Nil)

    /**
     * obtain a adjacency list representation of the graph
     *
     */
    def getAdjacencyList(): Array[Array[Int]] =
      {
        import scala.collection.mutable.HashMap

        // create a map from clusters to their indices
        val idMap = HashMap[Cluster, Int]()
        var nodelist = nodes
        for (i <- 0 until nodes.length) {
          val head = nodelist.head
          nodelist = nodelist.tail
          idMap.put(head, i)
        }

        // where we put the result
        val array = Array.fill(nodes.length)(List[Int]())

        // translating each link
        for (link <- links) {
          val h = idMap(link.source)
          val t = idMap(link.target)
          array(h) = t :: array(h)
        }

        array.map(_.distinct.toArray)
      }

    /**
     * obtain two adjacency list representation of the graph
     * one forward and one backward
     *
     */
    def getBiAdjacencyList(): (Array[Array[Int]], Array[Array[Int]]) =
      {
        import scala.collection.mutable.HashMap

        // create a map from clusters to their indices
        val idMap = HashMap[Cluster, Int]()
        var nodelist = nodes
        for (i <- 0 until nodes.length) {
          val head = nodelist.head
          nodelist = nodelist.tail
          idMap.put(head, i)
        }

        // where we put the result
        val forward = Array.fill(nodes.length)(List[Int]())
        val backward = Array.fill(nodes.length)(List[Int]())

        // translating each link
        for (link <- links) {
          val h = idMap(link.source)
          val t = idMap(link.target)
          forward(h) = t :: forward(h)
          backward(t) = h :: backward(t)
        }

        (forward.map(_.distinct.toArray), backward.map(_.distinct.toArray))
      }

    /**
     * topological sort
     *
     */
    def topoSort(): List[Cluster] =
      {
        val adjList = getAdjacencyList()

        for (i <- 0 until adjList.size) {
          println(nodes(i).name + ": " + i + " -> ")
          println("\t" + adjList(i).mkString(", "))
        }

        val n = adjList.length
        val visited = Array.fill(n)(false)
        val stack = Stack[Int]()

        for (i <- 0 until n) {
          if (!visited(i)) {
            topoSortUtil(i, adjList, visited, stack)
          }
        }

        stack.toList.map(nodes(_))

      }

    /**
     * topological sort with integers
     *
     */
    def topoSortInt(): List[Int] =
      {
        val adjList = getAdjacencyList()

        //        for (i <- 0 until adjList.size) {
        //          println(nodes(i).name + ": " + i + " -> ")
        //          println("\t" + adjList(i).mkString(", "))
        //        }

        val n = adjList.length
        val visited = Array.fill(n)(false)
        val stack = Stack[Int]()

        for (i <- 0 until n) {
          if (!visited(i)) {
            topoSortUtil(i, adjList, visited, stack)
          }
        }

        stack.toList
      }

    private def topoSortUtil(v: Int, adjacencyList: Array[Array[Int]], visited: Array[Boolean], stack: Stack[Int]) {
      visited(v) = true

      for (i <- 0 until adjacencyList(v).length) {
        val u = adjacencyList(v)(i)
        if (!visited(u)) {
          topoSortUtil(u, adjacencyList, visited, stack)
        }
      }

      stack.push(v)
    }

    /**
     * finds all the source nodes, i.e. nodes without temporal predecessors
     *
     */
    def findSources() = nodes.filterNot(n => links.exists(l => l.target == n))

    /**
     * finds all the sink nodes, i.e. nodes without temporal successors
     *
     */
    def findEnds() = nodes.filterNot(n => links.exists(l => l.source == n))

    /**
     * finds all middle nodes, i.e. nodes that are neither sources or sinks
     *
     */
    def findMiddle() = nodes.filter(n => links.exists(l => l.target == n || l.source == n))

    // this alias is used in XStreamable
    override def alias() = "plot-graph"

    //    def causalLinks() = links.filter(_.isCausal)
    //    def temporalLinks() = links.filter(_.isTemporal)
    def usedClusters() = links.flatMap { link => List(link.source, link.target) }.distinct

    def makeEfficient(): EfficientGraph =
      {
        new EfficientGraph(nodes, links)
      }

    /**
     * returns if cluster1 and 2 are ordered on the given graph, which is described by the links
     *
     */
    def ordered(cluster1: Cluster, cluster2: Cluster): Boolean =
      shortestDistance(cluster1, cluster2) != -1 || shortestDistance(cluster2, cluster1) != -1

    /**
     * find the shortest part distance between the two nodes based on the graph
     *  if source cannot be reached from target, return -1
     *
     */
    def shortestDistance(source: Cluster, target: Cluster): Int =
      {
        var debug = false
        //if (source.name == "choose restaurant" && target.name == "eat food") debug = true
        // a breadth-first search
        var longest = -1
        val queue = scala.collection.mutable.Queue[(Cluster, Int)]()
        var remaining = links
        queue += ((source, 0))

        while (queue != Nil) {
          val elem = queue.dequeue()

          val head = elem._1
          val dist = elem._2
          if (debug) println("dequeue: " + head.name + " " + dist)
          if (head == target) {
            return dist
          } else {
            links.filter(link => link.source == head).foreach {
              link =>
                queue.enqueue((link.target, dist + 1))
                if (debug) println("enqueue: " + link.target.name + " " + (dist + 1))
            }
          }
        }
        //println("distance from " + source.name + " to " + target.name + " = " + longest)
        -1
      }

    /**
     * find the diameter between the two nodes based on the graph
     *
     */
    def diameter(source: Cluster, target: Cluster): Int =
      {
        var debug = false
        //if (source.name == "choose restaurant" && target.name == "eat food") debug = true
        // a breadth-first search
        var longest = -1
        val queue = scala.collection.mutable.Queue[(Cluster, Int)]()
        var remaining = links
        queue += ((source, 0))

        while (queue != Nil) {
          val elem = queue.dequeue()

          val head = elem._1
          val dist = elem._2
          if (debug) println("dequeue: " + head.name + " " + dist)
          if (head == target) {
            if (dist > longest) longest = dist
          } else {
            links.filter(link => link.source == head).foreach {
              link =>
                queue.enqueue((link.target, dist + 1))
                if (debug) println("enqueue: " + link.target.name + " " + (dist + 1))
            }
          }
        }
        //println("distance from " + source.name + " to " + target.name + " = " + longest)
        longest
      }

    def takeSteps(source: Cluster, steps: Int): List[Cluster] =
      {
        var ends = List[Cluster]()

        val distance = scala.collection.mutable.Queue[(Cluster, Int)]()
        var remaining = links
        distance += ((source, 0))

        while (distance != Nil) {
          val elem = distance.dequeue()
          val head = elem._1
          val dist = elem._2
          if (dist == steps) ends = head :: ends // find one end. add it to the list
          else {
            links.filter(link => link.source == head).foreach {
              link => distance.enqueue((link.target, dist + 1))
            }
          }
        }

        ends.distinct
      }

    /**
     * eliminates redundant nodes, which do not appear in any links
     *
     */
    def reduce(): Graph =
      {
        val newNodes = usedClusters
        new Graph(newNodes, links, mutualExcls)
      }

    /**
     * simplifies the graph to a simple graph. When A->B and B->C are both present, A->C is omitted.
     *  The reduction only applies to links of the same kind
     */
    def simplify(): Graph = {

      // simplifying the graph
      val pairs = (0 until nodes.length) zip nodes
      val num2cluster = HashMap(pairs: _*) // mapping from number to cluster object
      val cluster2num = num2cluster.map(_.swap) // mapping from a cluster object to its number

      val temporals = links map {
        link =>
          val id1 = cluster2num(link.source)
          val id2 = cluster2num(link.target)
          (id1, id2)
      }

      //      val causals = links filter { _.isCausal } map {
      //        link =>
      //          val id1 = cluster2num(link.source)
      //          val id2 = cluster2num(link.target)
      //          (id1, id2)
      //      }

      // this is a helper function that delete redundant links
      def reduceLinks = (numbers: List[(Int, Int)]) => {

        if (numbers == Nil) Nil
        else {
          val order = new Ordering(numbers.toSet[(Int, Int)])
          order.necessary()
        }

      }

      val newLinks =
        reduceLinks(temporals).map { l => new Link(num2cluster(l._1), num2cluster(l._2)) }.toList

      new Graph(nodes, newLinks, mutualExcls)
    }

    // reduce + simplify
    def compact(): Graph = reduce.simplify

    // replace temporal with causal links so that there is at most one link between two clusters
    //    def singleLink(): Graph = {
    //      val causal = causalLinks()
    //      var temporal = temporalLinks()
    //
    //      temporal = temporal.filterNot { tl => causal.exists { c => c.target == tl.target && c.source == tl.source } }
    //
    //      new Graph(nodes, temporal ::: causal, mutualExcls)
    //    }

    /**
     * returns the direct predecessors of a graph node
     *
     */
    def predecessorsOf(c: Cluster): List[Cluster] =
      {
        if (!nodes.contains(c))
          throw new GraphException("The graph does not contain the node specified: " + c.name)
        else {
          links.filter(_.target == c).map(_.source)
        }
      }

    /**
     * remove nodes from the graph and any links involving these nodes
     */
    def removeNodes(excluded: List[Cluster]): Graph =
      {
        val newLinks = links.filterNot(l => excluded.contains(l.source) || excluded.contains(l.target))
        val newExcls = mutualExcls.filterNot(m => m.c1 == excluded || m.c2 == excluded)
        new Graph(nodes filterNot (excluded contains), newLinks, newExcls)
      }

    /**
     * add links from the predecessors of the events to the successors of the events
     *
     */
    def addSkipLinks(events: List[Cluster]): Graph =
      {
        val newLinks = skipLinks(events)
        new Graph(nodes, newLinks, this.mutualExcls)
      }

    def skipLinks(events: List[Cluster]): List[Link] = {
      var newLinks = ListBuffer[Link]() ++ links
      for (e <- events) {
        val predecessors = newLinks.filter(l => l.target == e).map(_.source)
        val successors = newLinks.filter(l => l.source == e).map(_.target)
        for (p <- predecessorsOf(e); s <- successors) {
          newLinks += new Link(p, s)
        }
      }
      newLinks.toList
    }

    /** generates a new graph by (1) detecting the optional and conditional events in the graph, 
     *  without the skip links
     */
    def graphWithOptionals(): Graph =
      {
        val (optionals, conditionals) = findOptionals()        
        new Graph(nodes, links, mutualExcls, optionals, conditionals)
      }
    
        /** generates a new graph by (1) detecting the optional and conditional events in the graph, 
     *  and (2) adds skip links for these events
     *
     */
    def graphWithOptionalsAndSkips(): Graph =
      {
        val (optionals, conditionals) = findOptionals()
        val canSkip = (optionals ::: conditionals).distinct
        val newLinks = skipLinks(canSkip)
        new Graph(nodes, newLinks, mutualExcls, optionals, conditionals)
      }

    /**
     * Finds the optional events and conditional events.
     *  Update: Only the first of the event pair becomes optional, the second is conditioned on the first event
     *
     */
    def findOptionals(): (List[Cluster], List[Cluster]) =
      {
        var optional = ListBuffer[Cluster]()
        var conditional = ListBuffer[Cluster]()

        // condition 1: c1 and c2 share a mutual exclusion but there is also a path from c1 to c2 on the graph
        val candidates = mutualExcls.filter(m => ordered(m.c1, m.c2)).map(m => (m.c1, m.c2))
        //println("candidates:\n" + candidates.mkString("\n"))
        // condition 2: c1 is not mutually exclusive to another (direct or indirect) predecessor of c2
        candidates.foreach {
          case (c1, c2) =>
            var early: Cluster = null
            var late: Cluster = null
            if (shortestDistance(c1, c2) != -1) {
              early = c1
              late = c2
            } else {
              early = c2
              late = c1
            }

            val prevented = mutualExcls.exists(m =>
              (m.c1 == early && m.c2 != late && shortestDistance(m.c2, late) != -1) ||
                (m.c2 == early && m.c1 != late && shortestDistance(m.c1, late) != -1))

            if (!prevented) {
              optional += early
              conditional += late
            }
        }

        (optional.distinct.toList, conditional.distinct.toList)
      }

    /**
     * draws the diagram as a png file
     *
     * @param fn The filename of the png file to be saved
     */
    def draw(fn: String) {

      val filename = fn + ".txt"
      val file = new File(filename)
      println(file.getCanonicalPath())
      val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream(file)))
      writer.println("digraph G {")

      for (node <- optionals) {
        writer.println("\"" + node.name + "\" [shape=box]")
      }
      //writer.println(causalLinks.map { l => "\"" + l.source.name + "\" -- \"" + l.target.name + "\" [style = \"dashed\"]" }.mkString("\n"))
      writer.println(links.map { l => "\"" + l.source.name + "\" -> \"" + l.target.name + "\"" }.mkString("\n"))

      writer.println(mutualExcls.map { m => "\"" + m.c1.name + "\" -> \"" + m.c2.name + "\" [style=dashed, dir=none]" }.mkString(";\n"))

      //writer.println(mutualExcls.map { m => "\"" + m.c1.name + """" -- [style = "dashed"]" """ + m.c2.name + "\""}.mkString(";\n"))      
      writer.println("}")
      writer.close()

      try {
        Runtime.getRuntime().exec("dot -Tpng -o" + fn + ".png " + filename)
        println("graph written to " + fn + ".png")
      } catch {
        case ioex: IOException =>
          print("Graph drawing failed: " + ioex.getMessage())
          println(". Possible Cause: Graphviz not installed properly.")
      }
      //file.deleteOnExit()
    }

    /**
     * draws the diagram as a png file with supplied substitution for vertex names
     *
     * @param fn The filename of the png file to be saved
     * @param dict The Map from the clusters to their new names
     */
    def drawWithNames(fn: String, dict: Map[Cluster, String]) {

      val filename = fn + ".txt"
      val file = new File(filename)
      println(file.getCanonicalPath())
      val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream(file)))
      writer.println("digraph G {")
      //writer.println(causalLinks.map { l => "\"" + l.source.name + "\" -- \"" + l.target.name + "\" [style = \"dashed\"]" }.mkString("\n"))
      writer.println(mutualExcls.map { m => "\"" + dict(m.c1) + "\" -> \"" + dict(m.c2) + "\" [style=dashed, dir=none]" }.mkString(";\n"))
      writer.println(links.map { l => "\"" + dict(l.source) + "\" -> \"" + dict(l.target) + "\"" }.mkString("\n"))
      //writer.println(mutualExcls.map { m => "\"" + m.c1.name + """" -- [style = "dashed"]" """ + m.c2.name + "\""}.mkString(";\n"))      
      writer.println("}")
      writer.close()

      println("writing graph to " + fn + ".png")
      Runtime.getRuntime().exec("dot -Tpng -o" + fn + ".png " + filename)
      file.deleteOnExit()
    }

    def numberNodes() = {
      var node2Num = Map[Cluster, Int]()
      var num2Node = Map[Int, Cluster]()

      var i = 0
      nodes.foreach {
        node =>
          node2Num += (node -> i)
          num2Node += (i -> node)
          i += 1
      }

      (num2Node, node2Num)
    }

    def containsLoop(): Boolean = {
      val length = nodes.size
      var visited = Array.fill[Boolean](length)(false)
      var stack = Stack[Int]()
      val (num2Node, node2Num) = numberNodes()

      stack.push(0)

      while (!stack.isEmpty) {
        val start = stack.top
        if (visited(start)) {
          // visiting the node for the second time
          stack.pop()
        } else {
          // visiting the node for the first time
          visited(start) = true

          val outgoing = links.filter(l => l.source == num2Node(start))
          outgoing.foreach {
            link =>
              //println("link = " + link.source.name + " " + link.target.name)
              val end = node2Num(link.target)
              // found a loop if we can reach one of the parent. It is visited so it is not a sibling
              if (stack.contains(end) && visited(end)) {
                //println("reached " + end + " from " + start)
                return true
              } else
                stack.push(end)
          }
        }
      }
      false
    }

    /**
     * find simple loops in the graph.
     * We assume that the loops in the graph does not share any edges
     */
    def simpleLoops(): List[List[Cluster]] = {
      val length = nodes.size
      var visited = Array.fill[Boolean](length)(false)
      var stack = Stack[Int]()
      val (num2Node, node2Num) = numberNodes()
      var loopList = List[List[Int]]()

      var parentTrack = Array.fill[Int](length)(-1)

      def runOneComponent(init: Int) {
        stack.push(init)

        while (!stack.isEmpty) {
          val start = stack.pop
          // visiting the node for the first time
          visited(start) = true
          //println("visit " + start)
          val outgoing = links.filter(l => l.source == num2Node(start))
          outgoing.foreach {
            link =>
              //println("link = " + link.source.name + " " + link.target.name)
              val end = node2Num(link.target)

              // find all parents
              var parents = List(start)
              var ptr = start
              while (parentTrack(ptr) != -1 && ptr != end) {
                ptr = parentTrack(ptr)
                parents = ptr :: parents
              }

              // found a loop if we can reach one of the parent. It is visited so it is not a sibling
              if (parents.contains(end)) {
                val loop = parents
                // add to loop list
                loopList = loop :: loopList
              } else if (!visited(end)) {
                stack.push(end)
                parentTrack(end) = start
              }
          }
        }
      }

      for (i <- 0 until length) {
        if (!visited(i))
          runOneComponent(i)
      }

      loopList.map(_.map(num2Node))
    }

    def allLoops(): List[List[Cluster]] = {
      val length = nodes.size
      var visited = Array.fill[Int](length)(0)
      var stack = Stack[Int]()
      val (num2Node, node2Num) = numberNodes()
      var loopList = List[List[Int]]()
      var curloop = 1 // current loop
      var parentTrack = Array.fill[Int](length)(-1)

      var parentStack = Stack[Int]()

      val init = 0 //math.floor(math.random * length).toInt
      println("init = " + init)
      stack.push(init)

      while (!stack.isEmpty) {
        val start = stack.pop
        // visiting the node for the first time
        visited(start) = curloop
        //println("visit " + start)
        val outgoing = links.filter(l => l.source == num2Node(start))
        outgoing.foreach {
          link =>
            //println("link = " + link.source.name + " " + link.target.name)
            val end = node2Num(link.target)

            // find all parents
            var parents = List(start)
            var ptr = start
            while (parentTrack(ptr) != -1 && ptr != end) {
              ptr = parentTrack(ptr)
              parents = ptr :: parents
            }

            // found a loop if we can reach one of the parent. It is visited so it is not a sibling
            if (parents.contains(end)) {
              //println("reached " + end + " from " + start)
              //var parents = stack.toList.filter(visited(_) == curloop).reverse
              // loop = parents list from end to start
              //val loop = parents.dropWhile(_ != end)
              val loop = parents
              //println("found loop: " + loop)
              // add to loop list
              loopList = loop :: loopList

              // mark all elements in the loop with a higher curloop
              //curloop += 1
              //loop.foreach(l => visited(l) = curloop)

            } else if (visited(end) < curloop) {
              stack.push(end)
              parentTrack(end) = start
              //println("push " + end)
              //parentStack.push(end)
            }
        }

      }

      loopList.map(_.map(num2Node))
    }

    def tarjan() {
      new Tarjan(this).run()
    }

  }

  class Tarjan(val graph: Graph) {

    val length = graph.nodes.size
    val (num2Node, node2Num) = graph.numberNodes()

    var highInd = 1;
    val stack: Stack[Int] = Stack[Int]()
    val lowlink = Array.fill[Int](length)(0)
    val index = Array.fill[Int](length)(0)

    var loopInd = 0
    var loopMember = scala.collection.mutable.HashMap[Int, List[Int]]()
    var loopList = List[List[Int]]()

    def run() {

      for (i <- 0 until length) {
        loopMember += (i -> List[Int]())
      }

      for (i <- 0 until length) {
        if (index(i) == 0) {
          strongconnect(i)

        }
      }

      println(loopMember.map {
        case (id, list) =>
          num2Node(id).name + " -> " + list
      })

      println("loop List = ")
      println(loopList.map(_.map(num2Node(_).name)))
    }

    private def strongconnect(v: Int) {
      index(v) = highInd
      lowlink(v) = highInd
      highInd = highInd + 1
      stack.push(v)

      println("visiting " + v)
      println("stack: " + stack)

      val node = num2Node(v)
      val outgoing = graph.links.filter(_.source == node)

      outgoing foreach {
        link =>
          val end = node2Num(link.target)

          println("links from " + v + " to " + end)
          if (index(end) == 0) {
            // end has not been visited
            strongconnect(end)
            if (lowlink(end) < lowlink(v)) {
              lowlink(v) = lowlink(end)
            }

          } else if (stack.contains(end)) {
            // this is the termination of recursion.
            // we have found a loop here            
            lowlink(v) = math.min(lowlink(v), lowlink(end))

            loopInd += 1

            // everything on the stack are included in the loop
            stack.foreach {
              k =>
                var list = loopMember(k)
                list = loopInd :: list
                loopMember(k) = list
            }

            loopList = stack.toList.reverse :: loopList
            println("all loops: " + loopList)
            //println(loopMember)
            println("from " + v + " reached " + end + ". loop!")
            //stack.pop()
          } else if (!loopMember(end).isEmpty) {
            val firstPart = stack.toList.reverse

            val secondLoops = loopList.filter(_.contains(end)).map(_.dropWhile(_ != end)).distinct
            println("s = " + secondLoops)
            secondLoops foreach {
              list =>
                val fullLoop = firstPart ::: list
                loopList = fullLoop :: loopList
                println("all loops: " + loopList)
            }

            println("from " + v + " reached " + end + ". loop!")
          }
      }

      stack.pop

      //      if (lowlink(v) == index(v)) {
      //        val component = ListBuffer[Int]()
      //        var w = -1
      //        do {
      //          w = stack.pop()
      //          component += w
      //        } while (w != v)
      //        println("component = " + component.map(x => num2Node(x).name).mkString("(", ", ", ")"))
      //      }
    }
  }

  object Graph {
    def nodesAfter(cluster: Cluster, links: List[Link]): List[Cluster] = {
      val after = ListBuffer(cluster)
      var added = ListBuffer(cluster)

      while (added != Nil) {
        val newAdded = ListBuffer[Cluster]()
        added foreach { c =>
          links foreach { link =>
            if (link.source == c)
              newAdded += link.target
          }
        }
        after ++= newAdded
        added = newAdded
      }

      after.toList
    }
  }
}