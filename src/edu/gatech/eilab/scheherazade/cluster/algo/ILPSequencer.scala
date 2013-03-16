package edu.gatech.eilab.scheherazade

import main._
import graph._
import data._
import scala.collection.mutable.ListBuffer
import utils.ilp.ILPOptimizer
import utils.Matrix

package cluster.algo {

  /**
   * Learning the sequence of clusters and the clusters at the same time, with the help from Jacob Eisenstein
   *
   * @author Albert Li
   */
  object ILPSequencer {

    def main(args: Array[String]) {
      // load the sentences and the gold
      val reader = new ConfigReader("configRobBest.txt")
      var (stories, gold) = reader.initData()

      stories = stories.take(20)
      val sentences = stories.flatMap(_.members)
      val (wordBags, words) = initSentencesAndWords(stories)
      val sentFreq = sentenceFreq(wordBags.values, words)

      //for (i <- 1 to 10) {
      //clusters = initClusters

      val i = 1
      NLPMain.switchDataSet("Robbery")
      val clusters = NLPMain.cluster(stories, 2)
      val clusFreq = clusterFreq(clusters, wordBags, words)

      val optimizer = new ILPOptimizer()
      optimizer.setParameters(transpose(sentFreq), transpose(clusFreq))

      var links = GraphGenerator.computeRelations(stories, clusters)
      val validLinks = links.sortWith { case (l1, l2) => l1.confidence > l2.confidence }.take(i)

      // constraints: sentences that cannot belong to certain clusters
      val disallowed = constrainMembership(validLinks, stories, clusters)
      println("sentences  = " + sentences.length)
      println("size = " + disallowed.length + ", " + disallowed(0).length)
      optimizer.addConstraint(disallowed)
      println("start solving...")
      val results = optimizer.solve

      Matrix.prettyPrint(results)

    }

    def constrainMembership(links: List[ObservedLink], stories: List[Story], clusters: List[Cluster]): Array[Array[Int]] =
      {
        val n = stories.map(_.members.size).sum
        val m = clusters.size
        val constraints = Array.ofDim[Int](n, m)

        var i = 0
        stories foreach { story =>
          for (sent <- story.members) {

            var innerCnt = 0 // sentence count within a story
            for (cluster <- clusters) {
              if (cluster contains sent) {
                val clustersAfter = Graph.nodesAfter(cluster, links)

                for (id <- (i - innerCnt) until i) // ids of sentences that come before the current sentence in the current story
                {

                  // find the ids of the clusters that come after the current cluster (i.e. the variable "cluster")
                  var cls = clusters
                  for (j <- 0 until m) {
                    if (clustersAfter contains cls.head)
                      constraints(id)(j) = 1
                    cls = cls.tail
                  }
                }
              }
            }
            innerCnt += 1
            i += 1
          }
        }

        constraints
      }

    def transpose(matrix: Array[Array[Double]]): Array[Array[Double]] =
      {
        val n = matrix.length
        val m = matrix(0).length
        val answer = Array.ofDim[Double](m, n)

        for (i <- 0 until n; j <- 0 until m) {
          answer(j)(i) = matrix(i)(j)
        }

        answer
      }

    def transpose(matrix: Array[Array[Int]]): Array[Array[Int]] =
      {
        val n = matrix.length
        val m = matrix(0).length
        val answer = Array.ofDim[Int](m, n)

        for (i <- 0 until n; j <- 0 until m) {
          answer(j)(i) = matrix(i)(j)
        }

        answer
      }

    def clusterFreq(clusters: List[Cluster], sentences: Map[Int, Array[String]], words: List[String]): Array[Array[Double]] =
      {
        val n = words.size
        val m = clusters.size

        println(n + " " + m)
        val freq = Array.ofDim[Double](n, m)

        var j = 0
        for (c <- clusters) {
          val myWords = ListBuffer[String]()

          // collect all words from all sentences in a cluster
          c.members.map { s => myWords ++= sentences(s.id) }

          var i = 0
          var count = 0
          words foreach { w =>
            val wordCount = myWords.count(_ == w)
            freq(i)(j) = wordCount
            count += wordCount
            i += 1 // next word, increment index
          }

          // normalize word count
          for (k <- 0 until n) {
            freq(k)(j) = freq(k)(j) / count
          }

          /*** debug info: printing all words present in the current cluster ***/
          //          println("cluster words : " + myWords.mkString(", "))
          //
          //          for (k <- 0 until n) {
          //            if (freq(k)(j) != 0)
          //              println("freq of " + words(k) + " = " + freq(k)(j))
          //          }
          //println()
          /*** debugging info ends ***/
          j += 1
        }

        freq
      }

    def initSentencesAndWords(stories: List[Story]): (Map[Int, Array[String]], List[String]) =
      {
        val stopwords = loadStopWords()
        var words = scala.collection.mutable.ListBuffer[String]()

        // extract the sentence id and words that have been regularized
        // stop words are removed
        val wordBags = stories.flatMap(_.members).map {
          sent =>
            (sent.id,
              sent.bagOfWords.filterNot(w => stopwords.contains(w)))
        }.toMap

        // count the regularized words
        wordBags.values foreach {
          _.foreach { word =>

            if (!(words contains word)) {
              words += word
              //println(word)
            }
          }
        }

        (wordBags, words.toList)
      }

    def sentenceFreq(sentences: Iterable[Array[String]], words: List[String]): Array[Array[Int]] =
      {

        val m = words.size // m is the height of the matrix
        val n = sentences.size // n is the width of the matrix
        val freq = Array.ofDim[Int](m, n)

        var i = 0;
        var j = 0;

        words foreach { w =>
          sentences foreach { s =>
            freq(i)(j) = s.count(_ == w)
            j += 1
          }
          j = 0
          i += 1
        }

        freq

        //printMatrix(words, sentences, freq)
      }

    def loadStopWords(): List[String] =
      {
        val list = scala.io.Source.fromFile("stopwords2.txt")
        list.getLines().toList
      }

    def printMatrix(words: Seq[String], sentences: List[Array[String]], freq: Array[Array[Int]]) {
      import java.io._
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("freqMatrix.csv")))
      // first line: only the sentences
      pw.println(" , " + sentences.map(_.mkString("", " ", "")).mkString("", ",", ""))
      // second line to end

      for (i <- 0 until freq.length) {
        pw.println(words(i) + ", " + freq(i).mkString("", ",", ""))
      }

      pw.close()
    }
  }
}