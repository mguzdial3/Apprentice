package edu.gatech.eilab.scheherazade.temp

import breeze.linalg._
import breeze.stats.distributions._
import java.io._
import scala.collection.mutable._
import scala.collection.mutable.Set

/**
 * Dirichlet Process with Gaussian base distribution and Gaussian data
 *
 */
object DPGaussian {

  val DATA_FILE = "SyntheticDPGaussian.txt"
  val base_mu = 0
  val baseSigma = 1.5
  val base = new Gaussian(base_mu, baseSigma)
  val generationSigma = 0.2
  val sigma = 0.2
  val ALPHA = 1

  def main(args: Array[String]) {
    //synthesizeData
    val data = readSyntheticData()
    MCMC2(data)
  }

  /**
   * algorithm 2 from Neal (2000)
   *
   */
  def MCMC2(data: Array[Double]) {
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("test.csv")))
    
    val MAX_K = 50000
    var n = data.length
    var c = Array.fill(n)(0)
    var state = ListBuffer[ListBuffer[Int]]()
    // initialization
    var theta = ListBuffer[Double]()

    //var book = HashMap[Set[Set[Int]], Int]()
    var bins = Array.fill(1001)(0)

    c(0) = 0
    state += ListBuffer(0)
    theta += base.draw

    for (iter <- 0 to MAX_K) {
      for (i <- 1 until n) {

        if (iter > 0) {
          // remove the ith point
          val cluster = state(c(i))
          if (cluster.size == 1) {
            state -= cluster
            theta -= theta(c(i))
            for (j <- 0 until n) {
              if (c(j) > c(i)) {
                c(j) -= 1
              }
            }
          } else {
            state(c(i)) -= i
            //println("removed " + i + " from " + c(i))
            //println(state)
          }
        }

        var probs = DenseVector.zeros[Double](state.size + 1)
        for (j <- 0 until state.size) {
          probs(j) = math.log(state(j).size) + logLikelihood(data(i), theta(j))
        }
        probs(state.size) = math.log(ALPHA) + logMarginalLikelihood(data(i))

        probs = normalizeLog(probs)

        c(i) = Multinomial(probs).draw

        if (c(i) < state.size) {
          // selecting an old cluster
          val cluster = state(c(i))
          cluster += i
          //println(i + " merged to cluster " + c(i))
          //println(state)
        } else {
          state += ListBuffer(i)
          theta += base.draw
          //println(i + " created new cluster " + c(i))
          //println(state)
        }

        // draw thetas
        for (i <- 0 until theta.size) {
          val dataInCluster = state(i).map(data(_)).toArray
          theta(i) = posterior(dataInCluster).draw
        }

        if (iter > 10000) {
          //setBookKeeping(state, book)
          for (i <- 0 until theta.size) {
            if (theta(i) < -5 || theta(i) > 5) {
              // discard anything < -5 or > 5
            } else {
              val bin = math.floor((theta(i) + 5) * 100).toInt
              bins(bin) += 1
            }
          }
        }
      }

      if (iter % 500 == 0) {
        println(iter)
        //        println(iter + ":" + state)
        //        println(theta.mkString(", "))
      }
    }

    //showSetBook(book, data)
    var list = List[(Double, Int)]()
    for(b <- 0 until bins.length)
    {
      val value = b / 100.0 - 5
      list = (value, bins(b)) :: list
    }
    list = list.sortBy(_._2 * -1)
    
    
    
    
    pw.println(list.take(200).map(p => p._1 + ", " + p._2).mkString("\n"))
    pw.close
  }

  def showSetBook(book: HashMap[Set[Set[Int]], Int], data: Array[Double]) {
    var sorted = book.toList.sortBy(p => -p._2)
    var d = 0
    while (sorted != Nil && d < 3) {
      val partition = sorted.head
      println(partition)
      for (p <- partition._1) {
        print("(")
        for (ind <- p) {
          print(data(ind))
          print(", ")
        }
        println(")")
      }
      //      

      sorted = sorted.tail
      d += 1
    }
  }

  def setBookKeeping(result: ListBuffer[ListBuffer[Int]], book: HashMap[Set[Set[Int]], Int]) {
    val hashset = Set[Set[Int]]()

    for (lbf <- result) {
      val set = Set[Int]()
      for (num <- lbf) {
        set += num
      }
      hashset += set
    }

    if (book.size > 20000) {
      // keep only the least frequent 1000 entries
      val sorted = book.toList.sortBy(p => -p._2)
      val remainder = sorted.take(1000)
      book.clear
      book ++= remainder
    } else {
      if (book contains hashset) {
        val count = book(hashset)
        book += ((hashset, count + 1))
      } else {
        book += ((hashset, 1))
      }
    }
  }

  def normalizeLog(vector: DenseVector[Double]): DenseVector[Double] =
    {
      val n = vector.size
      val sum = vector.sum
      val mean = sum / n

      val normalized = vector.map(v => math.exp(v - mean))
      normalized
    }

  /**
   * algorithm 1 from Neal (2000)
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
          theta(i) = posterior(Array(data(i))).draw
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

  def posterior(data: Array[Double]): Gaussian =
    {
      val sigmaSquared = sigma * sigma
      val baseSigmaSquared = baseSigma * baseSigma
      val n = data.length
      val dataSum = data.sum

      val postSigmaSquared = baseSigmaSquared * sigmaSquared / (n * baseSigmaSquared + sigmaSquared)

      val postMu = postSigmaSquared * (base_mu / baseSigmaSquared + dataSum / sigmaSquared)
      val postSigma = math.sqrt(postSigmaSquared)

      new Gaussian(postMu, postSigma)
    }

  def logLikelihood(x: Double, theta: Double): Double =
    {
      val sigmaSquared = sigma * sigma
      val coeff = 0.5 * math.log(2 * math.Pi * sigmaSquared)
      val c1 = -1 / (2 * sigmaSquared)
      val c2 = math.pow(x - theta, 2)

      coeff + c1 + c2
    }

  def logMarginalLikelihood(x: Double): Double =
    {

      val baseSigmaSquared = baseSigma * baseSigma
      val sigmaSquared = sigma * sigma

      val coeff = -0.5 * math.log(math.sqrt(2 * math.Pi * (baseSigmaSquared + sigmaSquared)))
      val c1 = (-x / (2 * sigmaSquared) - base_mu / (2 * baseSigmaSquared))
      val c2 = ((baseSigmaSquared * x * x / sigmaSquared + sigmaSquared * base_mu * base_mu / baseSigmaSquared + 2 * x * base_mu) /
        2 * (sigmaSquared + baseSigmaSquared))

      coeff + c1 + c2
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
      for (j <- 1 to 10) {
        val v = gs(i).draw
        pw.println(v)
      }
    }

    pw.close

  }

  def drawGaussian() =
    {
      val miu = base.draw

      Gaussian(miu, generationSigma)
    }
}