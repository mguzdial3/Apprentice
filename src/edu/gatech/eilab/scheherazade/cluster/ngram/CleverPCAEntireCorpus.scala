package edu.gatech.eilab.scheherazade.cluster.ngram

import breeze.linalg._
import java.io._
import edu.gatech.eilab.scheherazade.data.serialize.SevenZip
/**
 * perform PCA analysis on the entire n-gram data set
 *
 */
object CleverPCAEntireCorpus {
  
  def main(args: Array[String]) {
    val m = loadMatrix

    val (realValues, imaginaryValues, eigVectors) = eig(m)

    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("pcabasis.txt")))
    
    for (i <- 0 to 300) {
      pw.println(eigVectors(::, i).toArray.mkString(", "))      
    }
    
    pw.close
  }
  
  def loadMatrix():DenseMatrix[Double] = 
  {
    val matrix = DenseMatrix.zeros[Double](1000, 1000)
    for(i <- 1 to 4)
    {
      val fn = "xtx" + i + ".txt"
      val lines = scala.io.Source.fromFile(fn).getLines
      for (l <- lines)
      {
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

  def computeMatrix() {
    val store = new NGramMemory()
    val vectors = store.loadSparseMatrix

    // shift the means of the clusters to zero
    val ngrams = vectors(0).length
    val averages = Array.ofDim[Double](vectors.length)
    val sums = Array.ofDim[Double](vectors.length)
    (0 until vectors.length).par foreach { i =>

      sums(i) = accurateSum(vectors(i))
      averages(i) = sums(i) / ngrams
    }

    //val file = new File("averages.lzma")
    //SevenZip.write(file, averages.mkString("\n"))

    val matrix = DenseMatrix.zeros[Double](1000, 1000)

    val thread1 = new Runnable {
      override def run() {
        println("thread 1 working...")
        val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File("xtxThread1.txt"))))
        for (i <- 1 to 50) {
          val time = System.currentTimeMillis()
          for (j <- 0 to i) {
            val value = dot(vectors(i), averages(i), sums(i), vectors(j), averages(j), sums(j))
            pw.println(i + ", " + j + ", " + value)
          }
          println("thread1 1: " + i + " " + (System.currentTimeMillis - time) / 1000 + " secs")
          pw.flush()
        }
        pw.close
      }
    }

    val thread2 = new Runnable {
      override def run() {
        println("thread 2 working...")
        val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File("xtxThread2.txt"))))
        for (i <- 804 to 1000) {
          val time = System.currentTimeMillis()
          for (j <- 0 to i) {
            val value = dot(vectors(i), averages(i), sums(i), vectors(j), averages(j), sums(j))
            pw.println(i + ", " + j + ", " + value)
          }
          println("thread1 2: " + i + " " + (System.currentTimeMillis - time) / 1000 + " secs")
          pw.flush()
        }
        pw.close
      }
    }

    new Thread(thread1).start()
    //new Thread(thread2).start()

    //    for (i <- 0 to 1000; j <- i + 1 to 1000) {
    //      matrix(i, j) = matrix(j, i)
    //    }
    //


  }

}