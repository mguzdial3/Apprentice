package edu.gatech.eilab.scheherazade

import io._
import data._
import scala.collection.mutable.HashMap
package cluster.metric {

  /**
   * contains methods to evaluate cluster quality
   *
   */
  object ClusterMetric {

    /** evaluate clusters and print out results
     *  
     */
    def evaluate(clusters: List[Cluster], gold: List[Cluster]) = {

      val (r1, p1) = ClusterMetric.muc(gold, clusters)
      val (r2, p2) = ClusterMetric.bCubed(gold, clusters)
      println("MUC: precision " + p1 + " recall " + r1 + " f1 " + 2 * p1 * r1 / (p1 + r1))

      println("B Cubed: precision " + p2 + " recall " + r2 + " f1 " + 2 * p2 * r2 / (p2 + r2))
      val purity = ClusterMetric.purity(gold, clusters)
      println("purity: " + purity)
      
      (p1, r1, p2, r2, purity)
    }

    /**
     * @return average purity for all clusters
     */
    def purity(gold: List[Cluster], actual: List[Cluster]): Double = {
      val count: Double = actual.flatMap(_.members).size
      var total = 0
      for (c <- actual) {
        var max = 0
        for (g <- gold) {
          val matched = c.members.filter(s => g.members.contains(s)).size
          if (matched > max) max = matched
        }
        total += max
      }

      total / count
    }

    /**
     * @return recall and precision using the B cubed method
     *
     */
    def bCubed(gold: List[Cluster], actual: List[Cluster]): (Double, Double) =
      {
        val recall = computeBCubedPartition(gold, actual)
        val precision = computeBCubedPartition(actual, gold)

        (recall, precision)
      }

    private def computeBCubedPartition(base: List[Cluster], overlay: List[Cluster]): Double =
      {
        var sum: Double = 0 // numerator

        val allSents = base.flatMap(_.members)

        for (cluster <- base) {

          var numerator: Int = 0
          /* numerator for each cluster = 
         * sum over partition {sum over each sentence in a partition {cluster.length - partition.length}}
         */
          val partitions = partition(cluster, overlay)
          //println("partitions: " + partitions)
          for (curPar <- partitions) {
            //println("cur par = " + curPar)
            val item = curPar.length * (cluster.members.length - curPar.length)
            //println("item = " + item)
            numerator += item
          }
          sum += numerator.toDouble / cluster.members.length
        }
        1 - sum / allSents.length
      }

    /**
     * Evaluates the clustering results against the gold using the MUC method.
     *  @param gold the gold standard of clusters
     *  @param actual the actual clustering results
     *
     *  @return recall and precision using the MUC method
     */
    def muc(gold: List[Cluster], actual: List[Cluster]): (Double, Double) =
      {
        //println("computing recall")
        val recall = computeMUCPartition(gold, actual)
        //println("computing precision")
        val precision = computeMUCPartition(actual, gold)

        (recall, precision)
      }

    private def computeMUCPartition(base: List[Cluster], overlay: List[Cluster]): Double =
      {
        var total: Int = 0 // denominator
        var sum: Int = 0 // numerator

        for (cluster <- base) {
          val partitions = partition(cluster, overlay)
          //println("partition = " + partitions.map(_.map(_.id)))
          sum += (cluster.members.length - partitions.length)
          total += (cluster.members.length - 1)
        }

        sum / total.toDouble
      }

    private def partition(cluster: Cluster, overlay: List[Cluster]): List[List[Sentence]] =
      {
        val existing = overlay.map { o => cluster.members.filter { o.members.contains(_) } }.filterNot(_.isEmpty)
        val eElems = existing.flatMap(x => x)
        val singles = cluster.members.filterNot(eElems.contains(_))
        singles.map(List(_)) ::: existing
      }

    // this is the test case
    def main1(args: Array[String]) {
      //val storyFile = "movieHierarchical.txt"

      for (i <- 1 to 1) {
        //val tested = "robberyTotalManualClusters.txt"
        //val tested = "mv-cl-"+i+".txt" // cluster to be tested
        val tested = "./data/new_movie/georgePrelim/test.txt"

        val clusterFile = "./data/new_movie/georgePrelim/gold.txt"
        //val clusterFile = "./data/robbery/robberyGold.txt"

        var results: List[Cluster] = SimpleParser.parseClusters(tested)
        results = results.filter(c => c.members.size >= 4);

        println("using cluster file: " + clusterFile)
        val clusterList: List[Cluster] = SimpleParser.parseClusters(clusterFile).filter(c => c.members.size >= 4)

        //val results = storyList.map(s => new Cluster("a", s.members.toList))
        val (r1, p1) = muc(clusterList, results)
        val (r2, p2) = bCubed(clusterList, results)
        val pr = purity(clusterList, results)
        println("MUC: precision " + p1 + " recall " + r1 + " f1 " + 2 * p1 * r1 / (p1 + r1))

        println("B Cubed: precision " + p2 + " recall " + r2 + " f1 " + 2 * p2 * r2 / (p2 + r2))
        println("purity: " + pr);
      }

    }

    def main(args: Array[String]) {
      //val storyFile = "movieHierarchical.txt"

      //val tested = "robberyTotalManualClusters.txt"
      //val tested = "mv-cl-"+i+".txt" // cluster to be tested
      val tested = "./ngram-clusters-movie1.txt"

      val clusterFile = "./data/new_movie/movieGold2.gold"
      //val clusterFile = "./data/robbery/robberyGold.txt"

      var results: List[Cluster] = SimpleParser.parseClusters(tested)//.filter(c => c.members.size >= 4)
      results = results.filter(c => c.members.size >= 4);

      println("using cluster file: " + clusterFile)
      val clusterList: List[Cluster] = SimpleParser.parseClusters(clusterFile)//.filter(c => c.members.size >= 4)

//      results foreach {
//        gold =>
//          var best = 0
//          var bestMatch: Cluster = null
//          var best2nd = 0
//          var bestMatch2nd: Cluster = null
//          var cnt = 0
//
//          clusterList foreach {
//            gg =>
//              gold.members foreach { s1 =>
//                gg.members foreach {
//                  s2 => if (s1.id == s2.id) cnt += 1
//                }
//              }
//
//              if (cnt > best) {
//                best2nd = best
//                bestMatch2nd = bestMatch
//                best = cnt
//                bestMatch = gg
//              } else if (cnt > best2nd) {
//                best2nd = cnt
//                bestMatch2nd = gg
//              }
//
//          }
//
//          gold.members foreach {
//            s =>
//              if (bestMatch != null && !bestMatch.members.exists(m => m.id == s.id))
//                println("not matched: " + s.toSimpleString())
//          }
//
//      }

      //val results = storyList.map(s => new Cluster("a", s.members.toList))
      val (r1, p1) = muc(clusterList, results)
      val (r2, p2) = bCubed(clusterList, results)
      val pr = purity(clusterList, results)
      println("MUC: precision " + p1 + " recall " + r1 + " f1 " + 2 * p1 * r1 / (p1 + r1))

      println("B Cubed: precision " + p2 + " recall " + r2 + " f1 " + 2 * p2 * r2 / (p2 + r2))
      println("purity: " + pr);

    }

    def main2(args: Array[String]) {
      import scala.collection.mutable.ListBuffer
      //val storyFile = "movieHierarchical.txt"

      val tested = "./data/new_movie/georgePrelim/test.txt"
      val clusterFile = "./data/new_movie/georgePrelim/gold.txt"
      val realClusters = "./data/new_movie/movieGold2.txt"

      var results: List[Cluster] = SimpleParser.parseClusters(tested)
      results = results.filter(c => c.members.size >= 4);

      println("using cluster file: " + clusterFile)
      val georgeGold: List[Cluster] = SimpleParser.parseClusters(clusterFile).filter(c => c.members.size >= 4)

      val realGold: List[Cluster] = SimpleParser.parseClusters(realClusters).filter(c => c.members.size >= 4)

      /** adapt gold **/

      def depthEqual(s1: Sentence, s2: Sentence): Boolean =
        {
          if (s1.tokens.length != s2.tokens.length)
            return false
          else {
            for (i <- 0 until s1.tokens.length) {
              if (s1.tokens(i).word != s2.tokens(i).word)
                return false
            }
          }

          true
        }

      var idx = georgeGold.flatMap(_.members).map(_.id).max
      var adaptedGold = ListBuffer[Cluster]()
      realGold foreach {
        g =>
          var best = 0
          var bestCluster: Cluster = null
          var best2nd = 0
          var bestCluster2nd: Cluster = null
          var cnt = 0
          georgeGold foreach {

            gg =>
              g.members foreach { s1 =>
                gg.members foreach {
                  s2 => if (depthEqual(s1, s2)) cnt += 1
                }
              }

          }
      }

      //val results = storyList.map(s => new Cluster("a", s.members.toList))
      val (r1, p1) = muc(georgeGold, results)
      val (r2, p2) = bCubed(georgeGold, results)
      val pr = purity(georgeGold, results)
      println("MUC: precision " + p1 + " recall " + r1 + " f1 " + 2 * p1 * r1 / (p1 + r1))

      println("B Cubed: precision " + p2 + " recall " + r2 + " f1 " + 2 * p2 * r2 / (p2 + r2))
      println("purity: " + pr);

    }

    def initClusters(storyList: List[Story], clusterFile: String) =
      {
        val hashmap = new HashMap[Int, Sentence]
        storyList foreach {
          story =>
            story.members foreach
              {
                sentence =>
                  if (hashmap.contains(sentence.id)) throw new ParsingException("sentence repeated" + sentence.id)
                  hashmap += ((sentence.id, sentence))
              }
        }

        GoldParser.parseClusters(clusterFile) map {
          c =>
            val newMembers = c.members map
              {
                sentence =>
                  // make sure we get the same sentence 
                  hashmap.get(sentence.id).get
              }
            val newC = new Cluster(c.name, newMembers)
            newC.members foreach { s =>
              s.cluster = newC
            }
            newC
        }
      }

    def testBCubed1() {
      val sentences = (0 to 12).map(Sentence(_, Array(), 0))
      val c1 = new Cluster("a", List(sentences(0), sentences(1), sentences(2), sentences(3), sentences(4)))
      val c2 = new Cluster("b", List(sentences(5), sentences(6)))
      val c3 = new Cluster("c", List(sentences(7), sentences(8), sentences(9), sentences(10), sentences(11)))

      val truth = List(c1, c2, c3)

      val c4 = new Cluster("d", c1.members ::: c3.members)
      val product = List(c4, c2)

      val (r, p) = bCubed(truth, product)
      println(r + " " + p)
    }

    def testBCubed2() {
      val sentences = (0 to 12).map(Sentence(_, Array(), 0))
      val c1 = new Cluster("a", List(sentences(0), sentences(1), sentences(2), sentences(3), sentences(4)))
      val c2 = new Cluster("b", List(sentences(5), sentences(6)))
      val c3 = new Cluster("c", List(sentences(7), sentences(8), sentences(9), sentences(10), sentences(11)))

      val truth = List(c1, c2, c3)

      val c4 = new Cluster("d", c2.members ::: c3.members)
      val product = List(c4, c1)

      val (r, p) = bCubed(truth, product)
      println(r + " " + p)
    }

    def testMUC() {
      val sentences = (0 to 10).map(Sentence(_, Array(), 0))
      val c1 = new Cluster("a", List(sentences(0), sentences(1), sentences(2)))
      val c2 = new Cluster("b", List(sentences(3), sentences(4), sentences(5)))
      val c3 = new Cluster("c", List(sentences(6), sentences(7), sentences(8)))
      val truth = new Cluster("c", List(sentences(1), sentences(2), sentences(4), sentences(5), sentences(8), sentences(7), sentences(9)))

      val (r, p) = muc(List(truth), List(c1, c2, c3))
      println(r + " " + p)
    }

  }
}