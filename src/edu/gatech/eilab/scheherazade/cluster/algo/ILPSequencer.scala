package edu.gatech.eilab.scheherazade

import main._
import graph._
import data._
import scala.collection.mutable.ListBuffer
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

      NLPMain.switchDataSet("Robbery")
      var clusters = NLPMain.cluster(stories, 4)
      val numClusters = clusters.length

      println("Starting with OPTICS clusters: ")
      NLPMain.evaluate(clusters, gold)
      
      for (repeat <- 1 to 20) {
    	println("\nIteration " + repeat)
        val temp1 = GraphGenerator.computeRelations(stories, clusters).filter(_.confidence > 0.5)

        println("All available constraints: " + temp1)

        val goodRelations = temp1.take(repeat)
        val constraints = makeConstraints(goodRelations, clusters)
        println("Selected constraints: " + constraints)

        val buffers = Array.fill[ListBuffer[Sentence]](numClusters)(ListBuffer[Sentence]())

        for (story <- stories) {
          val sents = story.members
          print(".")
          //println("processing story = ")
          //println(sents.map(_.toSimpleString).mkString("\n"))

          val (wordBags, words) = initSentencesAndWords(sents.toList)
          //println("words" + words)
          val clusFreq = clusterFreq(clusters, allWordBags, words)
          val sentFreq = sentenceFreq(story.members.map { s => wordBags(s.id) }, words)

          //          println("sentence frequency: ")
          //          Matrix.prettyPrint(sentFreq)
          //          println("cluster frequency: ")
          //          Matrix.prettyPrint(clusFreq)
          //          readLine()

          val problem = new ILPProblem(sentFreq, clusFreq, constraints)
          val membership = problem.solve

          // collect results
          for (i <- 0 until story.members.length) {
            val sent = story.members(i)
            val index = membership(i)

            //println(sent.toSimpleString)
            //println("-> " + index)

            if (index != -1) {
              buffers(index) += sent
              //println(clusters(membership(i)).toHexSeparatedString)
            }
          }
        }

        val clusterList = ListBuffer[Cluster]()

        for (b <- buffers) {
          clusterList += new Cluster(b(0).toSimpleString, b.toList)
        }

        clusters = clusterList.toList

//        for (c <- clusters) {
//          println(c.toHexSeparatedString)
//        }
        
        
        println("iteration " + repeat + " completed")
        NLPMain.evaluate(clusters, gold)
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

    def makeConstraints(links: List[ObservedLink], clusters: List[Cluster]): List[(Int, Int)] =
      {
        links.map {
          l =>
            val int1 = clusters.indexOf(l.source)
            val int2 = clusters.indexOf(l.target)

            if (int1 == -1 || int2 == -1) {
              throw new RuntimeException("Warning: Cannot make constraints from non-existent clusters! \n" +
                "The link in question is " + l.toString)
            }

            (int1, int2)
        }
      }

    def constrainMembership(links: List[ObservedLink], stories: List[Story], clusters: List[Cluster]): Array[Array[Int]] =
      {
        val n = stories.map(_.members.size).sum
        val m = clusters.size
        val constraints = Array.fill[Int](n, m)(0)

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
        val answer = Array.fill[Double](m, n)(0)

        for (i <- 0 until n; j <- 0 until m) {
          answer(j)(i) = matrix(i)(j)
        }

        answer
      }

    def transpose(matrix: Array[Array[Int]]): Array[Array[Int]] =
      {
        val n = matrix.length
        val m = matrix(0).length
        val answer = Array.fill[Int](m, n)(0)

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
        val freq = Array.fill[Int](numClusters, numWords)(0)

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
        val freq = Array.fill[Int](numSents, numWords)(0)

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