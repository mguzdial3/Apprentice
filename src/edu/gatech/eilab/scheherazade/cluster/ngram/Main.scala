package edu.gatech.eilab.scheherazade

import nlp.NLPMain
import data._
import main._
import io._
import data.serialize._
import nlp._
import java.io._
import breeze.linalg._
import breeze.stats.distributions._
import cluster.metric.ClusterMetric
import scala.collection.mutable.ListBuffer

package cluster.ngram {

  /**
   *  Keeps the ngrams data for one corpus
   *  The ngram field is an 2d array. The first index is the sentence number and the array
   *  contains a number of ngrams in that sentence
   *
   *  For ngrams existing in the data, their corresponding vectors are stored in the vectors field
   *
   *  The stories field maps each story index to the list of indices of sentences the story contains
   */
  class NGramCorpus(val ngrams: Array[Array[String]], val vectors: Map[String, DenseVector[Double]], val stories: Array[Array[Int]]) extends XStreamable[NGramCorpus]

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

      Global.switchDataSet("Movie")
      val configFile = Global.configFile
      val parseFile = Global.parseFile

      val reader = new ConfigReader(configFile)
      var (stories, gold) = reader.initData()

      def parser() = CachedOperation {
        SFParser.parse(stories)
      }(Global.parseFile)

      stories = parser()
      val sents = stories.flatMap(_.members)

      def ngramFunc() = CachedOperation {

        ngramDB.init()
        //val string = "John took a piece of paper and made a paper plane" //"John opened the bank door"
       
        val list = stories.map {
          story =>

            story.members.map {
              sentence =>

                val ngrams = ngramize(sentence, ngramDB)
                (sentence.id, ngrams) //.map(x.filterNot(z => z.word == "John" || z.word == "Sally" || z.word == "and"))
            }
        }

        val map = scala.collection.mutable.HashMap[String, DenseVector[Double]]()

        val ngramsArray = Array.ofDim[Array[String]](sents.size)

        val sentIdMap = Array.ofDim[Array[Int]](list.length)

        for (storyId <- 0 until list.length) {
          
          sentIdMap(storyId) = list(storyId).map(_._1)
          
          for (j <- 0 until list(storyId).length) {
            val sentId = list(storyId)(j)._1
            
            val textList = list(storyId)(j)._2.getNGramsString()

            var validNGrams = List[String]()

            for (ng <- textList) {
              if (ngramDB.textExists(ng)) {
                validNGrams = ng :: validNGrams

                if (!map.contains(ng)) {

                  var dense = ngramDB(ng).toDenseVector

                  //dense = dense / dense.sum * (1 - 980 * VERY_SMALL)
                  /* the rational behind this normalization is that 
                 * this vector has only 20 non-zero components, so the rest will be VERY_SMALL                 
                 */

                  for (i <- 0 until dense.length) {
                    if (dense(i) == 0)
                      dense(i) = VERY_SMALL
                  }

                  //dense = dense / dense.sum

                  map += ((ng -> dense))
                }

              }
            }
            ngramsArray(sentId) = validNGrams.toArray
          }
        }

        new NGramCorpus(ngramsArray, map.toMap, sentIdMap)

      }(new File("MovieNgram.lzma"))

      println("reading ngram data...")

      var ngramCorpus = ngramFunc()

      /*
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("./data/vectors/movie.txt")))
      ngramCorpus = HMMModel2.performLargerPCA(ngramCorpus)
      for(pair <- ngramCorpus.vectors)
      {
        val ngram = pair._1
        pw.print(ngram)
        pw.print("\t")
        
        // count the number of occurrence
        val count = ngramCorpus.ngrams.map(s => s.count(_ == ngram)).sum
        pw.print(count)
        pw.print("\t")
        
        val vector = pair._2
        val str = vector.toArray.mkString("\t")
        pw.println(str)
      }*/
      
//      pw.println("s, dimension, MUC P, MUC R, B^3 P, B^3 R, Purity")
//      for (dimension <- 140 to 140 by 20) {
//        //GenModel2.DESIRED_DIMENSION = dimension
//        for (iteration <- 0 to 0) {
//          //GenModel2.ALPHA_SUM = dimension * 10 + 2 * dimension * iteration
          val foundClusters = cluster(sents, ngramCorpus)
          val noGarbage = foundClusters.filterNot(_.members.size < 4)
          val (p1, r1, p2, r2, purity) = ClusterMetric.evaluate(noGarbage, gold)
          //pw.println(ILPModel3.ALPHA_SUM + ", " + dimension + ", " + p1 + ", " + r1 + ", " + p2 + ", " + r2 + ", " + purity)
//        }
//
//        pw.flush
//      }

      //pw.close
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

    def cluster(sents: List[Sentence], corpus: NGramCorpus): List[Cluster] = {

      val clustering = NewGenModel.train(corpus)
      val length = clustering.max

      var clusters = List[Cluster]()
      for (i <- 0 to length) {
        var members = List[Sentence]()
        println("Cluster: " + i)
        for (s <- sents) {
          if (clustering(s.id) == i) {
            println(s.toSimpleString)
            members = s :: members
          }
        }
        println("*****\n")
        if (members != Nil) {
          val newCluster = new Cluster("C" + i, members)
          clusters = newCluster :: clusters
        }
      }

      clusters
    }

    def ngramize(sentence: Sentence, ngramDB: NGramStore): NGramData = {
      val queue = scala.collection.mutable.PriorityQueue[NGramData]()
      val root = NGramData.NGramDropOne(sentence)
      queue.enqueue(root)

      var found = false
      var solution: NGramData = null

      while (!found && !queue.isEmpty) {
        val s = queue.dequeue
        
        if (s.isComplete) {
          found = true
          solution = s
        } else {
          val splits = s.nextSplit(ngramDB)
          
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