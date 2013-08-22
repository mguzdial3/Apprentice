package edu.gatech.eilab.scheherazade.cluster.ngram

import breeze.linalg._
import java.io._
import edu.gatech.eilab.scheherazade.data.serialize.SevenZip
/**
 * perform PCA analysis on the entire n-gram data set
 *
 */
object PCAEntireCorpus {

  val maxSize = 30
  var cache = scala.collection.mutable.HashMap[Int, Array[Double]]()
  //var count = scala.collection.mutable.HashMap[Int, Double]()

  def main(args: Array[String]) {
    computeMatrix
  }

  def loadVector(idx: Int) = {
    if (cache.contains(idx)) {            
      cache(idx)
    } else {
      val filename = "c:/vectors/vector" + idx + ".lzma"
      val text = SevenZip.read(new File(filename))
      val answer = text.split("\n").map(_.toDouble)
      answer
    }
  }
  
  def loadCache(row:Int) {
    cache = scala.collection.mutable.HashMap[Int, Array[Double]]()
    //val half = maxSize / 2
    for (i <- row until row+maxSize)
    {
      cache += ((i -> loadVector(i)))
    }
  }

  /**
   * compute X transposed X
   *
   */
  def computeMatrix() {
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("XtX_n.csv")))
    val x_trans_x = new DenseMatrix[Double](1000, 1000)

    var i = 54
    loadCache(0)
    while (i < 1000) {      
        
      val time = System.currentTimeMillis()
      val vi = loadVector(i)
      var j = 0
      while (j <= i) {
        val vj = loadVector(j)
        x_trans_x(i, j) = dot(vi, vj)
        pw.println(i + ", " + j + ", " + x_trans_x(i, j))

        j += 1
      }
      println((System.currentTimeMillis() - time) / 1000 + " sec")
      println(i)
      pw.flush()

      i += 1
    }

    pw.close

    for (i <- 0 until 1000; j <- i + 1 until 1000) {
      x_trans_x(i, j) = x_trans_x(j, i)
    }

    printToFile(x_trans_x)
  }

  def dot(vi: Array[Double], vj: Array[Double]) =
    {
      var sum = 0.0
      for (i <- 0 until vi.length) {
        sum += vi(i) * vj(i)
      }

      sum
    }

  def saveVectors() {
    val store = new NGramMemory()
    val vectors = store.loadSparseMatrix

    // shift the means of the clusters to zero
    val ngrams = vectors(0).length
    (0 until vectors.length) foreach { i =>
      val time = System.currentTimeMillis()
      println("zero mean " + i)
      val mean = vectors(i).sum / ngrams
      val file = new File("vector" + i + ".lzma")

      var array: Array[Double] = null
      if (math.abs(mean) > 0.001) { // worth doing the zero mean computation
        var prev = 0
        array = Array.ofDim[Double](ngrams)
        for (pair <- vectors(i).activeIterator) {
          for (j <- prev until pair._1) {
            array(j) = (-1 * mean)
          }
          array(pair._1) = (pair._2 - mean)
          prev = pair._1 + 1
          //println("index = " +  pair._1)
        }
      } else // not worth it at all
      {
        array = vectors(i).toArray
      }

      SevenZip.write(file, array.mkString("\n"))
      println("vector " + i + "finished " + array.length)
      println("used " + (System.currentTimeMillis() - time) / 1000 + " secs")
      //pw.close

    }

  }

  def printToFile(M: DenseMatrix[Double]) {
    import java.io._
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("XtX.csv")))
    for (i <- 0 until 1000) {
      for (j <- 0 until 1000) {
        pw.print(M(i, j))
        if (j < 999) pw.print(", ")
      }
      pw.println
    }

    pw.close
  }
}