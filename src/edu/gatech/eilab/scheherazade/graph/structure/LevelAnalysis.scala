package edu.gatech.eilab.scheherazade.graph.structure
import edu.gatech.eilab.scheherazade.graph._
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

/** This code tries analyze a DAG by levels.
 *  Vertices in a DAG can be assigned to different levels where each edge goes from a higher level to a lower level (or a lower level to a higher level)
 *  
 *  Question: is this useful after all?
 *  
 *  author:Albert Li
 */

object LevelAnalysis {

  def main(args: Array[String]) {
    testLevelAssignment
  }

  def testLevelAssignment() {
    val graph = SampleGraph.sample3//.randomDAG(10, 15)
    graph.draw("unit-analysis")
    val lvlMap = assignLevelFromTop2(graph)
    lvlMap.foreach(
      p =>
        println(p._1.name + " : " + p._2))
  }

  /**
   * assigns a level number to each node in a DAG, such that each edge goes from a smaller level to a bigger level
   *  This method tries to assign the smallest number to each node. The assignLevelFromBottom method assigns the largest number to each node
   *  Note: the graph must not contain any cycles.
   */
  def assignLevelFromTop(graph: Graph): Map[Cluster, Int] =
    {
      val lvlMap = HashMap[Cluster, Int]()
      var queue = ListBuffer[(Cluster, Int)]()
      val sources = graph.findSources
      for (s <- sources) {
        queue += ((s, 0))
      }

      while (!queue.isEmpty) {
        // take the first element
        val head = queue.head
        val curCount = head._2
        val curNode = head._1
        queue = queue.tail

        graph.links.filter(l => l.source == curNode) foreach {
          l =>
            // enqueue
            val target = l.target
            queue = queue.filterNot(elem => elem._1 == target) += ((target, curCount + 1)) // push to the last position of the queue
        }

        if (lvlMap.contains(curNode)) {
          val oldCount = lvlMap(curNode)
          if (oldCount > curCount) {
            println("error in assigning small levels: old count > cur count " + curNode.name)
          }
        }
        lvlMap.put(curNode, curCount)
      }

      lvlMap.toMap
    }
  
  /** an alternative implementation
   *  
   */
  def assignLevelFromTop2(graph: Graph): Map[Cluster, Int] =
    {
      val lvlMap = HashMap[Cluster, Int]()
      val parentMap = HashMap[Cluster, Cluster]()
      val root = new Cluster("parent", Nil)
      lvlMap.put(root, -1)
      
      var queue = ListBuffer[Cluster]()
      val sources = graph.findSources
      for (s <- sources) {
        queue += s
        parentMap.put(s, root)
      }

      while (!queue.isEmpty) {
        // take the first element
        val head = queue.head
//        println("visit " + head.name)
        val curNode = head
        val curCount = lvlMap(parentMap(head)) + 1
        queue = queue.tail

        graph.links.filter(l => l.source == curNode) foreach {
          l =>
            // enqueue
            val target = l.target
            queue = queue.filterNot(_ == target) += target
            parentMap.put(target, curNode)
        }

        if (lvlMap.contains(curNode)) {
          val oldCount = lvlMap(curNode)
          if (oldCount > curCount) {
            println("error in assigning small levels: old count > cur count " + curNode.name)
          }
        }
        
        lvlMap.put(curNode, curCount)
      }

      lvlMap.toMap
    }

  /**
   *  assigns a level number to each node in a DAG, such that each edge goes from a smaller level to a bigger level
   *  This method tries to assign the largest number to each node. The assignLevelFromTop method assigns the smallest number to each node
   *  Note: the graph must not contain any cycles.
   */
  def assignLevelFromBottom(graph: Graph, maxLvl: Int): Map[Cluster, Int] =
    {
      val lvlMap = HashMap[Cluster, Int]()
      var queue = ListBuffer[(Cluster, Int)]()
      val sources = graph.findEnds
      for (s <- sources) {
        queue += ((s, maxLvl))
      }

      while (!queue.isEmpty) {
        // take the first element
        val head = queue.head
        val curCount = head._2
        val curNode = head._1
        queue = queue.tail

        graph.links.filter(l => l.target == curNode) foreach {
          l =>
            // enqueue
            val source = l.source
            queue = queue.filterNot(elem => elem._1 == source) += ((source, curCount - 1))
        }

        if (lvlMap.contains(curNode)) {
          val oldCount = lvlMap(curNode)
          if (oldCount < curCount) {
            println("error in assign big levels: old count < cur count " + curNode.name)
          }
        }
        lvlMap.put(curNode, curCount)
      }

      lvlMap.toMap
    }

  /**
   * finds the possible level ranges for each node in a DAG.
   *  Note the graph must not contain any cycles
   *  returns a map that maps each node to a pair of numbers: (lower-level, upper-level)
   */
  def possibleLevels(graph: Graph): Map[Cluster, (Int, Int)] =
    {
      val lvlRangeMap = HashMap[Cluster, (Int, Int)]()

      val lower = assignLevelFromTop(graph)
      val max = lower.values.max
      val upper = assignLevelFromBottom(graph, max)

      for (node <- graph.nodes) {
        val lowerBound = lower(node)
        val upperBound = upper(node)
        lvlRangeMap.put(node, (lowerBound, upperBound))
      }

      lvlRangeMap.toMap
    }
}

