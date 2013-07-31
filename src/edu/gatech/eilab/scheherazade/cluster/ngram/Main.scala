package edu.gatech.eilab.scheherazade

import nlp.NLPMain
import data._
import main._
import io._
import data.serialize._
import nlp._
import java.io.File
import breeze.linalg.SparseVector

package cluster.ngram {

  /**
   * keeps the ngrams data for one corpus
   *  Each NGramData in the ngrams field is a decomposed ngram for a sentence in the corpus
   *  For ngrams existing in the data, their corresponding vectors are stored in the vectors field
   */
  class NGramCorpus(val ngrams: List[NGramData], val vectors: Map[String, SparseVector[Float]]) extends XStreamable[NGramCorpus]

  object NGramizer {

    def main(args: Array[String]) {

      val ngramDB: NGramStore = new NGramMemory()

      Global.switchDataSet("Robbery")
      val configFile = Global.configFile
      val parseFile = Global.parseFile

      val reader = new ConfigReader(configFile)
      var (stories, _) = reader.initData()

      def parser() = CachedOperation {
        SFParser.parse(stories)
      }(Global.parseFile).flatMap(_.members)

      def ngramFunc() = CachedOperation {
        val sents = parser()

        ngramDB.init()
        //val string = "John took a piece of paper and made a paper plane" //"John opened the bank door"
        val list = sents.map {
          x => ngramize(x, ngramDB)
        }

        val map = scala.collection.mutable.HashMap[String, SparseVector[Float]]()

        for (n <- list) {
          val ngrams = n.getNGramsString()
          for (ng <- ngrams) {
            if (!map.contains(ng)) {
              map += ((ng -> ngramDB(ng)))
            }
          }
        }

        new NGramCorpus(list, map.toMap)

      }(new File("RobNgram.lzma"))

      val ngramCorpus = ngramFunc()

      for (nd <- ngramCorpus.ngrams) {
        val stringList = nd.getNGramsString
        for (string <- stringList) {
          val vector = ngramCorpus.vectors(string)
          println(string + ", " + vector)
        }
        println
        println("finished sentence " + nd.sentence.id)
      }

    }

    def ngramize(sentence: Sentence, ngramDB: NGramStore): NGramData = {
      val queue = scala.collection.mutable.PriorityQueue[NGramData]()
      val root = NGramData.NGramDropOne(sentence)
      queue.enqueue(root)

      var found = false
      var solution: NGramData = null

      while (!found && !queue.isEmpty) {
        val s = queue.dequeue
        //println("visiting: " + s.solutionString + " , cost = " + s.cost)
        if (s.isComplete) {
          found = true
          solution = s
        } else {
          val splits = s.nextSplit(ngramDB)
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
        throw new RuntimeException("queue exhausted!")
      } else {
        println("Solution = ")
        println(solution.solutionString)

        /* this portion finds equally good solutions. May not be necessary
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
        
        */

        solution
      }
    }
  }
}