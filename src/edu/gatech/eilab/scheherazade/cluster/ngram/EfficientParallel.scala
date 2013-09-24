package edu.gatech.eilab.scheherazade.cluster.ngram

import breeze.linalg._
import java.io._
import edu.gatech.eilab.scheherazade.data.serialize.SevenZip
/**
 * perform PCA analysis on the entire n-gram data set
 *
 */
object EfficientParallelPCA {

  val NUM_THREADS = 6

  def main(args: Array[String]) {
        val m = loadMatrix
    
        val (realValues, imaginaryValues, eigVectors) = eig(m)
    
        val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("pcabasis.txt")))
    
        for (i <- 0 to 998) {
          val vi = eigVectors(::, i)
          val vj = eigVectors(::, i+1)

//          print(realValues(i))
//          print(" ")
//          print(vi.dot(vi))
//          print(" ")
//          println(vi.dot(vj))
//          print(vi dot vi)
//          print(vi dot vi)
          pw.println(eigVectors(::, i).toArray.mkString(", "))
        }
    
        pw.close

    //computeMatrix()
  }

  def loadMatrix(): DenseMatrix[Double] =
    {
      val matrix = DenseMatrix.zeros[Double](1000, 1000)
      for (i <- 0 until NUM_THREADS) {
        val fn = "xtxThread" + i + ".txt"
        val lines = scala.io.Source.fromFile(fn).getLines
        for (l <- lines) {
          val array = l.split(",")
          val x = array(0).trim.toInt
          val y = array(1).trim.toInt
          val value = array(2).trim.toDouble

          matrix(x, y) = value
          matrix(y, x) = value
        }
      }

      matrix
    }

  /**
   * computes the dot product of (v_i - mi)^T (v_j - mj)
   *  (v_i - mi)^T (v_j - mj) = v_i^T v_j - sum(v_i)*mj - sum(v_j)*mi + mi*mj*length_of_vector
   */
  def dot(vi: SparseVector[Double], mi: Double, si: Double, vj: SparseVector[Double], mj: Double, sj: Double) =
    {
      //println("dimension = " + vi.length)
      var sum = 0.0
      sum += vi dot vj

      sum -= mi * sj
      sum -= mj * si
      sum += mi * mj * vi.length

      sum
    }

  /**
   * in order to compute the sum of a very large vector, perform:
   *  (1) split the vector into a number of buckets.
   *  (2) sum numbers in each bucket,
   *  (3) sum the sums for each bucket
   */
  def accurateSum(v: SparseVector[Double]): Double =
    {
      val buckets = 300
      var active = v.activeIterator.toArray
      val dimension = active.length
      val partial = Array.fill[Double](buckets)(0)
      val chuck = dimension / buckets
      println("chuck size = " + chuck)

      (0 to buckets - 2).par.foreach { i =>
        for (j <- i * chuck to ((i + 1) * chuck - 1)) {
          partial(i) += active(j)._2
        }
      }

      for (j <- (buckets - 1) * chuck until dimension) {
        partial(100) += active(j)._2
      }

      var sum = 0.0
      for (p <- partial) {
        sum += p
        //println(p)
        //println(pair._2)
      }
      //println("difference = " + (sum - v.sum))

      sum
    }

  /**
   * Compute the X transpose X matrix
   *
   */
  def computeMatrix() {
    // load the column vectors (i.e. one vector for each feature, not each data point)
    val store = new NGramMemory()
    val vectors = store.loadSparseMatrix

    // shift the means of the clusters to zero
    val ngrams = vectors(0).length
    val dimension = vectors.length
    val averages = Array.ofDim[Double](dimension)
    val sums = Array.ofDim[Double](dimension)
    (0 until dimension).par foreach { i =>

      sums(i) = accurateSum(vectors(i))
      averages(i) = sums(i) / ngrams
    }

    //val file = new File("averages.lzma")
    //SevenZip.write(file, averages.mkString("\n"))

    val matrix = DenseMatrix.zeros[Double](1000, 1000)

    // every thread makes use of this function
    def computeAndSave(thread: Int, start: Int, end: Int) {
      println("thread " + thread + " working...")
      val filename = "xtxThread" + thread + ".txt"
      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(filename))))

      for (i <- start to end) {
        val time = System.currentTimeMillis()
        for (j <- 0 to i) {
          val value = dot(vectors(i), averages(i), sums(i), vectors(j), averages(j), sums(j))
          pw.println(i + ", " + j + ", " + value)
        }
        println("thread " + thread + ": " + i + " " + (System.currentTimeMillis - time) / 1000 + " secs")
        pw.flush()
      }

      pw.close
    }

    // building the threads and run them
    val runnables = Array.ofDim[Runnable](NUM_THREADS)
    val totalTasks = (1 to NUM_THREADS).sum

    for (thread <- 0 until NUM_THREADS) {
      runnables(thread) = new Runnable {
        override def run() {
          val prevTasks = (NUM_THREADS until (NUM_THREADS - thread) by -1).sum

          val start = scala.math.round(1000.0 / totalTasks * prevTasks).toInt
          val end = scala.math.round(1000.0 / totalTasks * (prevTasks + NUM_THREADS - thread) - 1).toInt
          //println("Thread " + thread + " from " + start + " to " + end + ", prev = " + prevTasks)
          computeAndSave(thread, start, end)
        }
      }

      new Thread(runnables(thread)).start()
    }

  }

}