package edu.gatech.eilab.scheherazade.temp

/**
 * playing with Dirichlet Process
 *
 */

import breeze.linalg._
import breeze.stats.distributions._
import java.io._
import scala.collection.mutable._

object DiriProcess {

  val DATA_FILE = "SyntheticDirichlet.txt"
  val DIMENSION = 5
  val gamma = new Gamma(3, 0.1)
  val beta = new Beta(1, 1)
  val ALPHA: Double = 1.0

  def main(args: Array[String]) {
    //synthesizeData
    val data = readSyntheticData().map(DenseVector(_))
    //println(data.map(_.mkString(", ")).mkString("\n"))
    MCMC8(data)

    //    val firstTen = data.take(10)
    //    val mean = importanceSampleMean(firstTen)
    //    println(mean)
  }

  /**
   * algorithm 8 from Neal (2000)
   *
   */
  def MCMC8(data: Array[DenseVector[Double]]) {
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("nonconjugateDP.csv")))

    val MAX_K = 50000
    val BIN_MULTIPLIER = 100

    var n = data.length
    var c = Array.fill(n)(0)
    var state = ListBuffer[ListBuffer[Int]]()
    // initialization
    var theta = ListBuffer[Array[Double]]()
    val m = 5 // number of auxiliary variables

    //var bins = Array.fill(BIN_SIZE, BIN_SIZE, BIN_SIZE, BIN_SIZE, BIN_SIZE)(0)
    val book = HashMap[List[Int], Int]()

    c(0) = 0
    state += ListBuffer(0)
    theta += drawDirichletParams(DIMENSION)

    val extraTheta = {
      val temp = ListBuffer[Array[Double]]()
      for (i <- 0 until m) {
        temp += drawDirichletParams(DIMENSION)
      }
      temp
    }

    for (iter <- 0 to MAX_K) {
      //println(iter)
      for (i <- 1 until n) {

        if (iter > 0) {
          // remove the ith point
          val cluster = state(c(i))
          if (cluster.size == 1) {
            state -= cluster
            extraTheta(m - 1) = theta(c(i))
            theta -= theta(c(i))
            for (j <- 0 until n) {
              if (c(j) > c(i)) {
                c(j) -= 1
              }
            }

            //            println("deleted cluster " + c(i))
          } else {
            state(c(i)) -= i
            //            println("removed " + i + " from " + c(i))

          }
        }

        //        println(i)
        //        println(state)

        var probs = DenseVector.zeros[Double](state.size + m)

        for (j <- 0 until state.size) {
          probs(j) = math.log(state(j).size) + logDiriLikelihood(data(i), theta(j))
        }

        for (j <- state.size until state.size + m) {
          probs(j) = math.log(ALPHA / m) + logDiriLikelihood(data(i), extraTheta(j - state.size))
        }

        probs = normalizeLog(probs)

        c(i) = Multinomial(probs).draw

        if (c(i) < state.size) {
          // selecting an old cluster
          val cluster = state(c(i))
          cluster += i
//          println(i + " merged to cluster " + c(i))
//          println(state)
        } else {
          theta += extraTheta(c(i) - state.size)
          state += ListBuffer(i)
          c(i) = state.size - 1
          //          println(i + " created cluster " + c(i))
          //          println(state)
        }

        // draw thetas
        for (i <- 0 until theta.size) {
          val dataInCluster = state(i).map(data(_)).toArray
          theta(i) = importanceSample(dataInCluster)
        }

        if (iter > 4000) {
          //setBookKeeping(state, book)
          for (i <- 0 until theta.size) {
            if (theta(i).min < 0 || theta(i).max > 5) {
              // discard anything < -5 or > 5
              //println(theta(i))
            } else {
              val ind0 = math.floor(theta(i)(0) * BIN_MULTIPLIER).toInt
              val ind1 = math.floor(theta(i)(1) * BIN_MULTIPLIER).toInt
              val ind2 = math.floor(theta(i)(2) * BIN_MULTIPLIER).toInt
              val ind3 = math.floor(theta(i)(3) * BIN_MULTIPLIER).toInt
              val ind4 = math.floor(theta(i)(4) * BIN_MULTIPLIER).toInt

              val tuple = List(ind0, ind1, ind2, ind3, ind4)
              book.get(tuple) match {
                case Some(e) => book.update(tuple, e + 1)
                case None => book += ((tuple, 1))
              }
            }
          }
        }
      }

      if (iter % 500 == 0) {
        println(iter)

//        if (book.size > 200000) {
//          // keep only the least frequent 5000 entries
//          val sorted = book.toList.sortBy(p => -p._2)
//          val remainder = sorted.take(20000)
//          book.clear
//          book ++= remainder
//        }

      }
    }

    val top = book.toList.sortBy(p => -p._2).take(2000).map {
      case (tuple, count) =>
        (tuple.map(_.toDouble / BIN_MULTIPLIER), count)
    }

    pw.println(top.map(p => p._1.mkString(", ") + ", " + p._2).mkString("\n"))
    pw.close
  }

  def importanceSampleMean(data: Array[DenseVector[Double]]): DenseVector[Double] =
    {
      val trials = 100
      val theta = Array.ofDim[DenseVector[Double]](trials)
      var importance = DenseVector.zeros[Double](trials)
      for (i <- 0 until trials) {
        theta(i) = DenseVector(drawDirichletParams(DIMENSION))
        val diri = Dirichlet(theta(i))
        importance(i) = data.map {
          diri.logPdf
        }.sum
        //println(importance(i))
      }

      importance = normalizeLog(importance)
      val factor = importance.sum

      val vectorSum = DenseVector.zeros[Double](DIMENSION)
      for (i <- 0 until trials) {
        vectorSum += theta(i) * importance(i)
      }
      vectorSum / factor
    }

  def importanceSample(data: Array[DenseVector[Double]]): Array[Double] =
    {
      val trials = 40
      val theta = Array.ofDim[Double](trials, DIMENSION)
      val importance = DenseVector.zeros[Double](trials)
      for (i <- 0 until trials) {
        theta(i) = drawDirichletParams(DIMENSION)
        val diri = Dirichlet(theta(i))
        importance(i) = data.map {
          diri.logPdf
        }.sum

      }

      val probs = normalizeLog(importance)

      val choice = Multinomial(probs).draw
      theta(choice)
    }

  def normalizeLog(vector: DenseVector[Double]): DenseVector[Double] =
    {
      val n = vector.size
      val sum = vector.sum
      val mean = sum / n

      val normalized = vector.map(v => math.exp(v - mean))
      normalized
    }

  def logDiriLikelihood(data: DenseVector[Double], dirichlet: Array[Double]): Double =
    {
      val distr = Dirichlet(dirichlet)
      distr.logPdf(data)
    }

  def readSyntheticData(): Array[Array[Double]] =
    {
      val data = ArrayBuffer[Array[Double]]()
      val text = scala.io.Source.fromFile(DATA_FILE).getLines
      for (line <- text) {
        if (!line.startsWith("Dirichlet")) {
          val array = line.split("\\t").map(_.trim.toDouble)
          data += array
        }
      }

      data.toArray
    }

  def synthesizeData() {

    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(DATA_FILE)))

    val diri = Array.fill(4)(Dirichlet(drawDirichletParams(5)))

    for (i <- 0 until diri.length) {

      pw.print("Dirichlet: \t")
      pw.println(diri(i).params.toArray.mkString("\t"))
      for (j <- 1 to 10) {
        val v = diri(i).draw
        pw.println(v.toArray.mkString("\t"))
      }
    }

    pw.close

  }

  def drawDirichletParams(dimension: Int) =
    {
      val precision = 1 / gamma.draw
      var alpha = Array.fill(dimension)(beta.draw)
      val sum = alpha.sum
      alpha = alpha.map(_ * precision / sum)

      alpha
    }
}