package edu.gatech.eilab.scheherazade.temp

import breeze.linalg._
import breeze.stats.distributions._
import java.io._
import scala.collection.mutable._

/**
 * Dirichlet Process with Gaussian base distribution and Gaussian data
 *
 */
object DPGaussian {

  val DATA_FILE = "SyntheticDPGaussian.txt"
  val base_mu = 0
  val baseSigma = 2
  val base = new Gaussian(base_mu, baseSigma)
  val sigma = .5
  val ALPHA = 1.0

  def main(args: Array[String]) {
    //synthesizeData
    val data = readSyntheticData()
    MCMC1(data)
  }

  /** algorithm 1 from Neal (2000)
   *  
   */
  def MCMC1(data: Array[Double]) {
    val MAX_K = 5000
    var n = data.length
    var c = Array.fill(n)(0)
    // var count = Array.fill(MAX_K)(0)
    // initialization
    var theta = Array.fill(n)(0.0)

    var book = HashMap[List[Int], Int]()

    c(0) = 0
    theta(0) = base.draw
    for (i <- 1 until n) {
      val probs = DenseVector.zeros[Double](i + 1)
      for (j <- 0 to i - 1) {
        probs(j) = 1.0
      }
      probs(i) = ALPHA

      c(i) = Multinomial(probs).draw

      if (c(i) < i) {
        theta(i) = theta(c(i))
      } else {
        theta(i) = base.draw
      }

      //count(c(i)) += 1      
    }

    println("intialization: " + theta.mkString("\n"))

    for (iter <- 0 to MAX_K) {
      //println(iter)
      for (i <- 0 until n) {
        val probs = DenseVector.zeros[Double](n)

        for (j <- 0 until n if j != i) {
          probs(j) = new Gaussian(theta(j), sigma).pdf(data(i))
          //print(probs(j) + ", ")
        }

        probs(i) = marginalLikelihood(data(i)) * ALPHA
        //println(probs(i))

        c(i) = Multinomial(probs).draw
        if (c(i) != i) {
          theta(i) = posterior(data(i)).draw
        } else {
          theta(i) = theta(c(i))
        }

        //println("state: " + theta.mkString(","))
        if (iter > 2000)
          keepRecord(book, theta)
      }

    }

    var sorted = book.toList.sortBy(p => -p._2)
    var d = 0
    while (sorted != Nil && d < 3) {
      println(sorted.head)
      sorted = sorted.tail
      d += 1
    }

  }

  def keepRecord(book: HashMap[List[Int], Int], theta: Array[Double]) {
    val n = theta.length

    var list = ListBuffer[Double]()
    var cluster = Array.ofDim[Int](n)

    for (i <- 0 until n) {
      val index = list.indexOf(theta(i))
      if (index == -1) {
        cluster(i) = list.size
        list += theta(i)
      } else {
        cluster(i) = index
      }
    }

    val cl = cluster.toList

    if (book.contains(cl)) {
      val count = book(cl)
      book += ((cl, count + 1))
    } else {
      book += ((cl, 1))
    }
  }

  def posterior(x: Double): Gaussian =
    {
      val sigmaSquared = sigma * sigma
      val baseSigmaSquared = baseSigma * baseSigma

      val postSigmaSquared = baseSigmaSquared * sigmaSquared / (baseSigmaSquared + sigmaSquared)

      val postMu = postSigmaSquared * (base_mu / baseSigmaSquared + x / sigmaSquared)
      val postSigma = math.sqrt(postSigmaSquared)

      new Gaussian(postMu, postSigma)
    }

  def marginalLikelihood(x: Double): Double =
    {

      val baseSigmaSquared = baseSigma * baseSigma
      val sigmaSquared = sigma * sigma

      val coeff = 1.0 / math.sqrt(2 * math.Pi * (baseSigmaSquared + sigmaSquared))
      val c1 = math.exp(-x / (2 * sigmaSquared) - base_mu / (2 * baseSigmaSquared))
      val c2 = math.exp((baseSigmaSquared * x * x / sigmaSquared + sigmaSquared * base_mu * base_mu / baseSigmaSquared + 2 * x * base_mu) /
        2 * (sigmaSquared + baseSigmaSquared))

        
      coeff * c1 * c2
    }

  def readSyntheticData(): Array[Double] =
    {
      val data = ArrayBuffer[Double]()
      val text = scala.io.Source.fromFile(DATA_FILE).getLines
      for (line <- text) {
        if (!line.startsWith("Gaussian")) {
          val v = line.trim.toDouble
          data += v
        }
      }

      data.toArray
    }

  def synthesizeData() {

    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream(DATA_FILE)))

    val gs = Array.fill(4)(drawGaussian)

    for (i <- 0 until gs.length) {

      pw.print("Gaussian: \t")
      pw.println(gs(i).mu + ", " + gs(i).sigma)
      for (j <- 1 to 5) {
        val v = gs(i).draw
        pw.println(v)
      }
    }

    pw.close

  }

  def drawGaussian() =
    {
      val miu = base.draw

      Gaussian(miu, sigma)
    }
}