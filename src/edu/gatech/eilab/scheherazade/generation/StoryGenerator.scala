package edu.gatech.eilab.scheherazade
import main._
import data._
import graph._
import analysis._
import io._
import scala.collection.mutable.ListBuffer
import java.io._
import graph.passage._

package generation {

  object StoryGenerator {

    def main(args: Array[String]) {
      val story = genStory()
      val text = story.map(_.name).mkString("\n")
      println("--------------------------------")
      println(text)
    }

    def genStory(): List[Cluster] = {
      val reader = new ConfigReader("configAffairsBest.txt")
      var (stories, clusters) = reader.initData()

      val para = reader.properties.allParameters()(0)

      val minimumSize = para.intParam("minClusterSize")
      val insideClusters = clusters.filterNot(c => c.members.size < minimumSize)
      val insideStories = reader.filterUnused(stories, insideClusters)

      val gen = new GraphGenerator(insideStories, insideClusters)
      var graph: Graph = gen.generateQIP(para)("mutualExcl")._1

      graph.draw("aaaa")
      var walk = Passage.init(graph)

      //Walk.debug = true
      //for (i <- 0 to 10)
      //LFFWalk(walk)
      randomWalk(walk)
    }

    /**
     *   always selecting the most frequent even from the fringe
     */
    def MFFWalk(firstWalk: AbstractPassage): List[Cluster] = {
      var trace = List[Cluster]()

      var walk = firstWalk

      while (walk.hasMoreSteps) {
        val fringe = walk.fringe

        val lfStep = fringe.maxBy(cluster => cluster.size)
        //val i = math.floor(math.random * fringe.size).toInt
        //val step = fringe(i)

        trace = lfStep :: trace
        walk = walk.forward(lfStep)
      }

      trace.reverse
    }

    /**
     *   always selecting the least frequent event from the fringe
     */
    def LFFWalk(firstWalk: AbstractPassage): List[Cluster] = {
      var trace = List[Cluster]()

      var walk = firstWalk

      while (walk.hasMoreSteps) {
        val fringe = walk.fringe

        val lfStep = fringe.minBy(cluster => cluster.size)
        //val i = math.floor(math.random * fringe.size).toInt
        //val step = fringe(i)

        trace = lfStep :: trace
        walk = walk.forward(lfStep)
      }

      trace.reverse
    }

    def randomWalk(firstWalk: AbstractPassage):List[Cluster] = {
      //println(); println()
      var story = List[Cluster]()
      
      var walk = firstWalk

      while (walk.hasMoreSteps) {
        val fringe = walk.fringe
        val i = math.floor(math.random * fringe.size).toInt
        val step = fringe(i)

        story = step :: story
        
        walk = walk.forward(step)
      }
      story.reverse
    }

    def bruteSearch(firstWalk: AbstractPassage) {
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("valid stories.txt")))

      var q = scala.collection.mutable.Queue[AbstractPassage]()
      var good: Long = 0
      var result = scala.collection.mutable.Set[AbstractPassage]()
      q.enqueue(firstWalk)

      while (!q.isEmpty) {
        var n = q.dequeue()

        //println(n)
        //println("fringe: " + n.fringe.map(_.name).mkString("(", ", ", ")"))
        //println("\n\n")    

        //println("story length = " + n.history.size)
        if (!n.hasMoreSteps()) {
          // we have reached the end.
          result += n
        } else {
          n.fringe foreach { step =>
            q += n.forward(step)
          }
        }

        n = null
      }

      //for (story <- result)
      //pw.println("GOOD STORY: \n" + story)

      pw.close()

      println("found " + result.size + " stories.")
      println("Considered " + firstWalk.totalGenerated + " search nodes. ")
    }
  }
}

