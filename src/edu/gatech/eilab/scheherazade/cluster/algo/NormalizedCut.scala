package edu.gatech.eilab.scheherazade

import data._
import Jama._
import scala.collection.mutable.ListBuffer

package cluster.algo {

  object NormalizedCut {

    def cluster(similarity: Array[Array[Double]], sentences: List[Sentence], k: Int): List[Cluster] =
      {
        val n = similarity.length
        val sents = sentences.toArray
        val ids = normalizedCut(similarity, k)
        val ans = ListBuffer[Cluster]()

        for (i <- 1 to k) {
          val members = ListBuffer[Sentence]()

          for (j <- 0 until n) {
            if (ids(j) == i)
              members += sents(j)
          }
          val c = new Cluster("Cluster " + i, members.toList)
          ans += c
        }

        ans.toList
      }

    protected def normalizedCut(similarity: Array[Array[Double]], k: Int): Array[Int] = {
      val n = similarity.length

      for (i <- 0 until n) {
        var sum = 0.0
        for (j <- 0 until n)
          sum += similarity(i)(j)

        for (j <- 0 until n)
          similarity(i)(j) = similarity(i)(j) / sum
      }

      for (i <- 0 until n) {
        similarity(i)(i) = 1 - similarity(i)(i)
      }

      // normalized laplacian matrix
      val laplacian = new Matrix(similarity)

      val eig = new EigenvalueDecomposition(laplacian)
      val eigVectors = eig.getV().transpose().getMatrix(0, k - 1, 0, n - 1)

      kmeans(eigVectors, k)
    }

    /**
     * k-means clustering. returns an array of integers
     *  array(i) = j indicates that the ith element is in the jth cluster
     *  j ranges from 1 to k
     */
    def kmeans(features: Matrix, k: Int): Array[Int] = {
      val dimension = features.getRowDimension()
      val n = features.getColumnDimension()

      println("d = " + dimension + ", n = " + n)

      val results = Array.ofDim[Int](n)

      var changed = true
      var it = 0; // number of iteration
      val maxIt = 250; // max iterations

      val centroids = Array.ofDim[Double](k, dimension)

      // random initialization
      for (i <- 0 until dimension) {
        var min = 1.0
        var max = 0.0

        for (j <- 0 until n) {
          val elem = features.get(i, j)
          if (min > elem)
            min = elem
          if (max < elem)
            max = elem
        }

        for (j <- 0 until k) {
          centroids(j)(i) = math.random * (max - min) + min
        }
      }

      // iterating until no change or max iteration reached
      while (it < maxIt && changed) {
        changed = false
        
        val sumCluster = Array.fill(k, dimension)(0.0)
        val numCluster = Array.fill(k)(0)
        
        for (i <- 0 until n) {
          val vector = features.getMatrix(0, dimension - 1, i, i).getColumnPackedCopy()
          // which centroid is nearest
          val nearest = (0 to k).minBy(x => eucDist(centroids(x), vector))
          // if nearest != results(i), the result has changed
          changed = changed || (nearest != results(i))
          results(i) = nearest
          
          // update the stats
          numCluster(nearest) += 1
          for(j <- 0 until dimension)
            sumCluster(nearest)(j) += vector(j)
        }
        
        // update the centroids
        for(i <- 0 until k; j <- 0 until dimension)
        {
          centroids(i)(j) = sumCluster(i)(j) / numCluster(i)
        }
      }

      results
    }

    /**
     * Euclidean Distance
     *
     */
    def eucDist(a: Array[Double], b: Array[Double]): Double =
      {
        var sum = 0.0
        for (i <- 0 until a.length) {
          val s = a(i) - b(i)
          sum += s * s
        }
        math.sqrt(sum)
      }

    def randomMatrix(n: Int) =
      {
        Array.fill[Double](n, n) {
          math.random
        }
      }

    def main(args: Array[String]) {
      val n = 5
      val similarity = randomMatrix(n)

    }
  }

}