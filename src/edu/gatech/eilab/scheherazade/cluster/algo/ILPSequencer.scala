package edu.gatech.eilab.scheherazade

import main._
import graph._
import data._
import scala.collection.mutable.ListBuffer
import utils.ilp.ILPProblem
import utils.Matrix
import scala.collection.immutable.Traversable

package cluster.algo {

  /**
   * Learning the sequence of clusters and the clusters at the same time, with the help from Jacob Eisenstein
   *
   * @author Albert Li
   */
  object ILPSequencer {

    def main(args: Array[String]) {
      // load the sentences and the gold
      val reader = new ConfigReader("configRob.txt")
      var (stories, gold) = reader.initData()

      val sentences = stories.flatMap(_.members)
      val (allWordBags, allWords) = initSentencesAndWords(sentences)
      //val sentFreq = sentenceFreq(wordBags.values, words)

      //for (i <- 1 to 10) {

      //      val i = 1
      OPTICS.visualized = false
      NLPMain.switchDataSet("Robbery")
      val clusters = NLPMain.cluster(stories, 2)

      for (story <- stories) {
        val sents = story.members
        println("processing cluster = ")
        println(sents.map(_.toSimpleString).mkString("\n"))

        val (wordBags, words) = initSentencesAndWords(sents.toList)
        println("words" + words)
        val clusFreq = clusterFreq(clusters, allWordBags, words)
        val sentFreq = sentenceFreq(story.members.map { s => wordBags(s.id) }, words)

        println("sentence frequency: ")
        Matrix.prettyPrint(sentFreq)
        println("cluster frequency: ")
        Matrix.prettyPrint(clusFreq)
        readLine()

        val problem = new ILPProblem(sentFreq, clusFreq, Nil)
        val membership = problem.solve

        // output results
        var i = 0
        for (sent <- story.members) {
          println(sent.toSimpleString)
          println("-> " + membership(i))
          if (membership(i) != -1)
            println(clusters(membership(i)).toHexSeparatedString)

          i += 1
        }
        readLine()
      }
      //
      //      val problem = new ILPProblem()
      //      optimizer.setParameters(transpose(sentFreq), transpose(clusFreq))
      //
      //      var links = GraphGenerator.computeRelations(stories, clusters)
      //      val validLinks = links.sortWith { case (l1, l2) => l1.confidence > l2.confidence }.take(i)
      //
      //      // constraints: sentences that cannot belong to certain clusters
      //
      //      val results = optimizer.solve
      //
      //      Matrix.prettyPrint(results)

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

    def clusterFreq(clusters: List[Cluster], sent2Words: Map[Int, Array[String]], words: List[String]): Array[Array[Int]] =
      {
        val numWords = words.size
        val numClusters = clusters.size

        //println(n + " " + m)
        val freq = Array.ofDim[Int](numClusters, numWords)

        var clist = clusters

        for (i <- 0 until numClusters) {
          // for a given cluster
          val c = clist.head
          clist = clist.tail

          // collect all words from all sentences in a cluster
          val myWords = c.members.flatMap { s => sent2Words(s.id) }

          var wlist = words
          //println("my words = " + myWords)
          for (j <- 0 until numWords) {
            val w = wlist.head
            wlist = wlist.tail
            // for each possible word

            val wordCount = myWords.count(_ == w)
            freq(i)(j) = wordCount
            //println("for word " + w + " = " + wordCount)
            //readLine()
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
        }

        freq
      }

    /**
     * returns a hashmap (sentence_id -> Array of words in that sentence) and a list of all used words
     *
     */
    def initSentencesAndWords(sentences: List[Sentence]): (Map[Int, Array[String]], List[String]) =
      {
        val stopwords = loadStopWords()
        var words = scala.collection.mutable.ListBuffer[String]()

        // extract the sentence id and words that have been properly tokenized (performed in sent.bagOfWords)
        // stop words are removed
        val wordBags = sentences.map {
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

    /**
     * count the words from the sentences
     * @return a matrix of number-of-sentences by number-of-words. Each cell
     * contains the number of times the word occurs in that sentence
     */
    def sentenceFreq(sentences: Array[Array[String]], allWords: List[String]): Array[Array[Int]] =
      {

        val numWords = allWords.size // m is the height of the matrix
        val numSents = sentences.size // n is the width of the matrix
        val freq = Array.ofDim[Int](numSents, numWords)

        for (i <- 0 until numSents) {
          val s = sentences(i)
          var j = 0
          for (w <- allWords) {
            freq(i)(j) = s.count(_ == w)
            j += 1
          }
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