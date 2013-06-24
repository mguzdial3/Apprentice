package edu.gatech.eilab.scheherazade

import nlp.NLPMain
import data._
import main._

package cluster.ngram {
  object NGramizer {

    def main(args: Array[String]) {
      NGramData.createReadConnection()

      Global.switchDataSet("Robbery")
      val configFile = Global.configFile
      val parseFile = Global.parseFile

      val reader = new ConfigReader(configFile)
      var (stories, _) = reader.initData()
      //val parser = new StoryNLPParser(stories, parseFile, true)
      //val sents = parser().storyList.flatMap(_.members)

      //val string = "John took a piece of paper and made a paper plane" //"John opened the bank door"
//      sents.take(50).foreach{
//        x => ngramize(x)
//      }

    }

    def ngramize(sentence:Sentence) {
      val queue = scala.collection.mutable.PriorityQueue[NGramData]()
      val root = NGramData.NGramDropOne(sentence)
      queue.enqueue(root)

      var found = false
      var solution: NGramData = null

      while (!found && !queue.isEmpty) {
        val s = queue.dequeue
        //println("visiting: " + s.solutionString + " , cost = " + s.cost)
        if (s.complete) {
          found = true
          solution = s
        } else {
          val splits = s.nextSplit
          //println("  nextSplits = ")
          for (sp <- splits) {
            //println("    " + sp.solutionString + " , cost = " + sp.cost)
            queue.enqueue(sp)
          }
          //println("queue head = " + queue.head.solutionString + ", " + queue.head.cost)
          //queue.enqueue (splits : _*)
        }
      }

      if (!found) {
        println("queue exhausted!")
      } else {
        println("Solution = ")
        println(solution.solutionString)

        val c = solution.cost
        while (found && !queue.isEmpty) {
          val k = queue.dequeue()
          if (k.cost == solution.cost && k.isComplete) {
            println("  " + k.solutionString)
          } else {
            println("  2nd best: " + k.solutionString)
            found = false
          }
        }
      }
    }
  }
}