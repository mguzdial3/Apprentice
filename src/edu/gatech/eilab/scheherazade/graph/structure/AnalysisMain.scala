package edu.gatech.eilab.scheherazade.graph.structure
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import edu.gatech.eilab.scheherazade.generation._

object AnalysisMain {

  def main(args: Array[String]) {

    val graph = regularize(SampleGraph.randomDAG(20, 70, 4))
    val sGraph = simplifyGraph(graph)
    val originalCount = countStories(graph)
    println("original # = " + originalCount)
    val simplifiedCount = countStories(sGraph)
    println("simplified # = " + simplifiedCount)
    println("ratio = " + simplifiedCount.toDouble / originalCount)
  }

  /**
   * (1) adds optional and conditional events and (2) add mutual exclusions between the endings
   *  (3) adds START and END events
   *
   */
  def regularize(graph: Graph): Graph =
    {

      var ends = graph.findEnds
      var newME = List[MutualExcl]()
      while (ends != Nil) {
        val head = ends.head
        ends = ends.tail
        for (te <- ends) {
          newME = new MutualExcl(head, te) :: newME
        }
      }

      var newGraph = new Graph(graph.nodes, graph.links, newME ::: graph.mutualExcls)

      addStartEnd(newGraph).graphWithOptionalsAndSkips
    }

  def countStories(graph: Graph, mustHave:List[Cluster]): Long = {
    import java.io._
    val ends = graph.findEnds
    val firstWalk = WalkOnDisk.fromInits(graph.findSources, graph, graph.mutualExcls, graph.optionals)

    val dir = new File("./stats")
    if (!dir.exists()) dir.mkdir()

    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("./stats/valid stories.txt")))

    var q = scala.collection.mutable.Stack[WalkOnDisk]()
    var total: Long = 0
    var good: Long = 0
    val known = new KnownElements(10000)

    q.push(firstWalk)

    while (!q.isEmpty) {
      var n = q.pop()

      if (ends.exists(c => n.history.contains(c)) || n.fringe == Nil) {
        // we have reached the end. 
        // end nodes are considered to be mutually exclusive.
        // you can only reach one any time
        //        val string = compactString(n)
        //        //println("produced story " + string)
        //        val check = known.check(string)
        //        if (check) {
        //          pw.println(string)
        good += 1
        //        }

        //          val story = decode(string)
        //          println("found **\n" + story)
        if (n)
      } else {
        q pushAll (n.oneStep(graph.mutualExcls, graph.optionals))
      }

//      n = null
    }
    good
  }

  /**
   * test out all simplification techniques
   *
   */
  def simplifyGraph(graph: Graph): Graph = {
    var newGraph = graph
    newGraph.draw("unit-analysis")

    var clans = UnitAnalysis.findClans(newGraph)
    println("clans = " + clans.mkString(", "))
    val meNodes = newGraph.mutualExcls.flatMap(me => List(me.c1, me.c2)).distinct
    val rndInd = math.floor(math.random * meNodes.size).toInt
    val background = meNodes(rndInd)
    //println("background = " + background.name)

    newGraph = MutexAnalysis.cleanedGraph(graph, List(background), clans)
    newGraph.draw("mutex-analysis")

    var numCol = 0
    clans = UnitAnalysis.findClans(newGraph)
    while (clans != Nil) {
      println("clans = " + clans.mkString(", "))
      newGraph = UnitAnalysis.collapseClans(newGraph, clans)
      //println("collapsing...")
      numCol += 1
      //newGraph.draw("after-collapsing-" + numCol)
      clans = UnitAnalysis.findClans(newGraph)
    }

    newGraph.draw("after-collapsing")

    //    val closures = UnitAnalysis.findClosures(graph)
    //    println(closures.mkString(", "))
    newGraph
  }

  /**
   * adds pseudo events START and END
   *
   */
  def addStartEnd(graph: Graph): Graph =
    {
      val start = new Cluster("START", Nil)
      val end = new Cluster("END", Nil)

      val links = ListBuffer[Link]()

      for (e <- graph.findSources) yield {
        val l = new Link(start, e)
        links += l
      }

      for (e <- graph.findEnds) yield {
        val l = new Link(e, end)
        links += l
      }

      new Graph(start :: end :: graph.nodes, graph.links ::: links.toList, graph.mutualExcls, graph.optionals, graph.conditionals)
    }

  def testClans() {
    import java.io._
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("random record.csv")))

    for (nodes <- 10 to 20) {
      val all = nodes * (nodes - 1) / 2
      val least = math.floor(all * 0.5).toInt
      val most = math.floor(all * 0.6667).toInt
      for (links <- least to most) {
        var total = 0
        var numNodes = 0
        for (i <- 1 to 100) {
          val graph = SampleGraph.randomDAG(nodes, links, 3)
          //val graph = SampleGraph.sample5
          //graph.draw("random")
          val clans = UnitAnalysis.findClans(graph)
          total += clans.size
          numNodes += clans.map(_.size).sum
        }

        //pw.println(nodes + ", " + links + ", " + total/100.0 + ", " + numNodes / 100.0)
        pw.println(nodes + ", " + links.toDouble / all + ", " + numNodes / 100.0 / nodes)
        println(nodes + ", " + links + ", " + total / 100.0 + ", " + numNodes / 100.0)
        println(nodes + ", " + links.toDouble / all + ", " + numNodes / 100.0 / nodes)
      }
    }

    pw.close

  }

}