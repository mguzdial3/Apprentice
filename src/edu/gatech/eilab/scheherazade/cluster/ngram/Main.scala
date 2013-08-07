package edu.gatech.eilab.scheherazade

import nlp.NLPMain
import data._
import main._
import io._
import data.serialize._
import nlp._
import java.io.File
import breeze.linalg._
import breeze.stats.distributions._

package cluster.ngram {

  /**
   * keeps the ngrams data for one corpus
   *  Each NGramData in the ngrams field is a decomposed ngram for a sentence in the corpus
   *  For ngrams existing in the data, their corresponding vectors are stored in the vectors field
   */
  class NGramCorpus(val ngrams: Array[Array[String]], val vectors: Map[String, DenseVector[Double]]) extends XStreamable[NGramCorpus]

  object NGramizer {

    def main(args: Array[String]) {
      //      for (i <- 0 to 10) {
      //        val v = math.random
      //        println(v)
      //      }
      val corpus = produceNGrams()
      //      val assignments = GenModel.train(corpus)
    }

    def produceNGrams() = {

      val VERY_SMALL = 1e-8

      val ngramDB: NGramStore = new NGramMemory()

      Global.switchDataSet("Robbery")
      val configFile = Global.configFile
      val parseFile = Global.parseFile

      val reader = new ConfigReader(configFile)
      var (stories, _) = reader.initData()

      def parser() = CachedOperation {
        SFParser.parse(stories)
      }(Global.parseFile).flatMap(_.members)

      val sents = parser()

      def ngramFunc() = CachedOperation {

        ngramDB.init()
        //val string = "John took a piece of paper and made a paper plane" //"John opened the bank door"
        val list = sents.map {
          x => ngramize(x, ngramDB)
        }

        val map = scala.collection.mutable.HashMap[String, DenseVector[Double]]()

        val array = Array.ofDim[Array[String]](list.size)
        var i = 0

        for (n <- list) {
          val textList = n.getNGramsString()
          var finalGrams = List[String]()
          for (ng <- textList) {
            if (ngramDB.textExists(ng)) {
              finalGrams = ng :: finalGrams

              if (!map.contains(ng)) {

                var dense = ngramDB(ng).toDenseVector

                dense = dense / dense.sum * (1 - 980*VERY_SMALL)

                for (i <- 0 until dense.length) {
                  if (dense(i) == 0)
                    dense(i) = VERY_SMALL
                }

                //dense = dense / dense.sum

                map += ((ng -> dense))
              }

            }
          }
          array(i) = finalGrams.toArray
          i += 1
        }

        new NGramCorpus(array, map.toMap)

      }(new File("RobNgram.lzma"))

      println("reading ngram data...")

      val ngramCorpus = ngramFunc()

      cluster(sents, ngramCorpus)

    }

    def testProbabilities(corpus: NGramCorpus) {
      var list = List[Double]()
      for (word <- corpus.vectors) {

        //val word = ngramCorpus.vectors.head
        println(word._1)
        var text = word._2
        //println(word._2)
        //println(word._2.size)

        text = text / text.sum

        var v = DenseVector.rand(1000)
        v = v / (v.sum * 0.002) // * 0.00066677) //* 0.00066)

        if (text.sum == 0) {
          println("Warning")
          System.exit(1)
        }
        //println("v = " + v)
        val p = new Dirichlet(v).logPdf(text)
        println(p)
        list = p :: list
      }

      val mean = list.sum / list.size
      println("min = " + list.min)
      println("max = " + list.max)
      println("average = " + mean)
      println("deviation = " + (mean - list.min) + ", " + (list.max - mean) + ", " + (list.max - list.min))

    }

    def cluster(sents:List[Sentence], corpus: NGramCorpus) {
      val clustering = GenModel.train(corpus)
      val length = clustering.max

      for (i <- 0 to length) {
        println("Cluster: " + i)
        for (s <- sents) {
          if (clustering(s.id) == i) {
            println(s.toSimpleString)
          }
        }

        println("*****\n")
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