package edu.gatech.eilab.scheherazade.graph.structure
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import edu.gatech.eilab.scheherazade.generation._
import edu.gatech.eilab.scheherazade.main.Global

object AnalysisMain {

  def main(args: Array[String]) {

    import java.io._
    Global.graphDrawing = false

    //    val pw = new PrintWriter("mutexAnalysis.csv")

//    val before = SampleGraph.sample20
//    before.draw("before")
//    val graph = AnalysisMain.regularize(before)
//    graph.draw("after")
//    val background = graph.nodes(4)
//    val queryCluster = graph.nodes(5)

    //        val before = SampleGraph.sample18
    //        val graph = AnalysisMain.regularize(before)
    //        val background = graph.nodes(7)
    //        val queryCluster = graph.nodes(3)
    //        Thread.sleep(10000)
    //System.exit(1)
        var i = 1
        var noMistake = true
        var ratio = 0.0
        while (i < 1000 && noMistake) {
          i += 1
          val graph = regularize(SampleGraph.randomDAG(10, 30, 4))
          val (background, queryCluster) = generateQuery(graph)

//    println("background cluster = " + background.name)
//    println("query cluster = " + queryCluster.name)
//    println("optionals = " + graph.optionals)
//    println("conditionals = " + graph.conditionals)
//    
    //val originalEnds = graph.findEnds 
  
    val (cGraph, sGraph) = simplifyGraph(graph, List(background))

    
    val (originalTotal, originalGood, originalQuery) = countStories(graph, List(background), List(queryCluster))
    println("original total = " + originalTotal + ", good = " + originalGood + " query = " + originalQuery + " ratio = " + originalQuery.toDouble / originalGood)

    
    //TODO: if the cleaned graph does not contain query, then the probability is directly zero

    val (cleanTotal, cleanGood, cleanQuery) = countStories(cGraph, List(background), List(queryCluster))
    println("cleaned total = " + cleanTotal + ", good = " + cleanGood + " query = " + cleanQuery + " ratio = " + cleanQuery.toDouble / cleanGood)
    //      pw.println(cleanTotal.toDouble / originalTotal)
//    testStory(cGraph)

    ratio += cleanTotal / originalTotal.toDouble
    if (originalQuery.toDouble / originalGood != cleanQuery.toDouble / cleanGood) {
      println("!!!!!!!!mistake!!!!!!!")
      val original = recordStories(graph, List(background), List(queryCluster))
      val mutex = recordStories(cGraph, List(background), List(queryCluster))
      println("in original graph = \n" + original.filterNot(mutex.contains).mkString("\n"))
      println("in mutex graph = \n" + mutex.filterNot(original.contains).mkString("\n"))

              println("failed after " + i)
              noMistake = false
              Global.graphDrawing = true
              graph.draw("unit-analysis")
              cGraph.draw("mutex-analysis")
              graph.compact.draw("unit-analysis-compact")
              cGraph.compact.draw("mutex-analysis-compact")
              println("background cluster = " + background.name)
              println("query cluster = " + queryCluster.name)
              println("optionals = " + graph.optionals)
              println("conditionals = " + graph.conditionals)
      
              println("-----------------------------")
              countStories(cGraph, List(background), List(queryCluster), true)
            }
    }
    //
    if (noMistake) {
      println("finished a hundred")
    }
    println("ratio = " + ratio / 1000)

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

      var newGraph = new Graph(graph.nodes, graph.links, (newME ::: graph.mutualExcls).distinct)

      val g = addStartEnd(newGraph.graphWithOptionalsAndSkips)
      g
    }

  def testStory(graph: Graph) {
    val ends = graph.findEnds
    var walk = WalkOnDisk.fromInits(graph.findSources, graph, true)
    var fringe = walk.fringe

    while (fringe != Nil) {
      println("Select from the following: ")
      var i = 1
      for (e <- fringe) {
        println(i + ": " + e.name)
        i += 1
      }
      println("your choice: ")
      var choice = 0
      while (choice <= 0 || choice > fringe.size) {
        try {
          choice = readLine().toInt
        } catch {
          case e: Exception => println(e.getMessage)
        }
      }

      val step = fringe(choice - 1)
      println("you selected " + step.name)

      walk = walk.takeStep(step)
      fringe = walk.fringe
      walk.selfGraph.draw("walk-graph")
    }
  }

  def countStories(graph: Graph, background: List[Cluster], query: List[Cluster], debug: Boolean = false): (Long, Long, Long) = {
    val ends = graph.findEnds
    //println("ends = " + ends)
    val firstWalk = WalkOnDisk.fromInits(graph.findSources, graph)

    var q = scala.collection.mutable.Stack[WalkOnDisk]()
    var totalCnt: Long = 0
    var goodCnt: Long = 0
    var queryCnt: Long = 0

    q.push(firstWalk)

    while (!q.isEmpty) {
      var n = q.pop()

      //if (!n.hasMoreSteps) {
      if (ends.exists(c => n.history.contains(c)) || n.fringe == Nil) {
        // we have reached the end. 
        // end nodes are considered to be mutually exclusive.
        // you can only reach one any time

        totalCnt += 1

        val nameSeq = n.history.flatMap {
          _ match {
            case clan: ClanCluster =>
              clan.clusters.map(_.name)
            case c: Cluster =>
              List(c.name)
          }
        }

        //        if (debug) {
        //          println("made story:" + nameSeq.reverse.mkString(" "))
        //        }
        //println("made story:" + nameSeq.reverse.mkString(" "))

        if (background.forall(c => nameSeq.contains(c.name))) {
          goodCnt += 1

          if (query.forall(c => nameSeq.contains(c.name))) {
            queryCnt += 1
          }
        }
      } else {
        q pushAll (n.possibleSteps())
      }

      //      n = null
    }
    (totalCnt, goodCnt, queryCnt)
  }

  def recordStories(graph: Graph, background: List[Cluster], query: List[Cluster]): List[List[String]] = {
    val ends = graph.findEnds
    val firstWalk = WalkOnDisk.fromInits(graph.findSources, graph)

    var goodStories = List[List[String]]()

    var q = scala.collection.mutable.Stack[WalkOnDisk]()
    var totalCnt: Long = 0
    var goodCnt: Long = 0
    var queryCnt: Long = 0

    q.push(firstWalk)

    while (!q.isEmpty) {
      var n = q.pop()

      if (ends.exists(c => n.history.contains(c)) || n.fringe == Nil) {
        // we have reached the end. 
        // end nodes are considered to be mutually exclusive.
        // you can only reach one any time

        totalCnt += 1

        val nameSeq = n.history.flatMap {
          _ match {
            case clan: ClanCluster =>
              clan.clusters.map(_.name)
            case c: Cluster =>
              List(c.name)
          }
        }

        if (background.forall(c => nameSeq.contains(c.name))) {
          goodCnt += 1
          goodStories = nameSeq.reverse :: goodStories

          if (query.forall(c => nameSeq.contains(c.name))) {
            queryCnt += 1
          }
        }
      } else {
        q pushAll (n.possibleSteps())
      }

      //      n = null
    }
    goodStories
  }

  /**
   * randomly generates a query for the story understanding problem
   *  A query contains one event that must occur, and another event whose probability we want
   */
  def generateQuery(graph: Graph) =
    {
      var meNodes = graph.mutualExcls.flatMap(me => List(me.c1, me.c2)).distinct
      val ind1 = math.floor(math.random * meNodes.size).toInt
      val background = meNodes(ind1)
      meNodes = meNodes.filterNot(_ == background)

      var ind2 = math.floor(math.random * meNodes.size).toInt
      val target = meNodes(ind2)

      (background, target)
    }

  /**
   * test out all simplification techniques
   *
   */
  def simplifyGraph(graph: Graph, keptNodes: List[Cluster]): (Graph, Graph) = {
    var newGraph = graph
    newGraph.draw("unit-analysis")

    //println("background = " + background.name)

    val cleanedGraph = MutexAnalysis.cleanedGraph(newGraph, keptNodes)
    cleanedGraph.draw("mutex-analysis")

    var tGraph = cleanedGraph
    var numCol = 0
    var clans = UnitAnalysis.findClans(tGraph)
    while (clans != Nil) {
      //      println("clans = " + clans.mkString(", "))
      tGraph = UnitAnalysis.collapseClans(tGraph, clans)
      //println("collapsing...")
      numCol += 1
      //newGraph.draw("after-collapsing-" + numCol)
      clans = UnitAnalysis.findClans(tGraph)
    }

    tGraph.draw("after-collapsing")

    //    val closures = UnitAnalysis.findClosures(graph)
    //    println(closures.mkString(", "))
    (cleanedGraph, tGraph)
  }

  /**
   * adds pseudo events START and END
   *
   */
  def addStartEnd(graph: Graph): Graph =
    {
      val start = new Cluster("START", Nil)
      val end = new Cluster("END", Nil)

      var newLinks = graph.links

      for (e <- graph.findSources) {
        val l = new Link(start, e)
        newLinks = l :: newLinks
        // need to handle the case where the sources are optional. Note sources cannot be conditional
        if (graph.optionals.contains(e)) {
          val successors = newLinks.filter(l => l.source == e).map(_.target)
          newLinks = successors.map(new Link(start, _)) ::: newLinks
        }
      }

      for (e <- graph.findEnds) {
        val l = new Link(e, end)
        newLinks = l :: newLinks
        // need to handle the case where the ends are conditional. Note ends cannot be optional
        if (graph.conditionals.contains(e) || graph.optionals.contains(e)) { // changed to include optionals
          val predecessors = newLinks.filter(l => l.target == e).map(_.source)
          newLinks = predecessors.map(new Link(_, end)) ::: newLinks
        }
      }

      new Graph(start :: end :: graph.nodes, newLinks.distinct, graph.mutualExcls, graph.optionals, graph.conditionals)
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