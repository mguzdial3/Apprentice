package edu.gatech.eilab.scheherazade.temp

/**
 * playing with the Breeze library
 *
 */
import breeze.linalg._
import breeze.stats.distributions._
import java.io._
import cc.mallet.optimize._

object Play {

  def main(args: Array[String]) {
    testMax()
  }
  
  def testMax() {
    val matrix = DenseMatrix.zeros[Double](4, 4)
    matrix(1, 2) = 4
    matrix(3, 2) = 1
    matrix(0, 2) = 20.5
    matrix(3, 1) = 1
    matrix(1, 1) = 2.3
    matrix(2, 2) = 1
    matrix(0, 3) = 12
    
    println(matrix.max)
  }

  def testWordNet() {
    System.setProperty("wordnet.database.dir", "./wordnet/dict/")
    val wordnet = edu.smu.tspell.wordnet.WordNetDatabase.getFileInstance();
    wordnet.getSynsets("fly").length > 0

  }

  def testStringBuilder() {
    val builder = new StringBuilder()
    builder += 'a'
    builder += 'b'
    builder += 'c'
    println(builder.toString)
  }

  def testArgMax() {
    val vec = SparseVector.zeros[Double](500)
    vec(4) = -1
    vec(3) = 5
    vec(2) = 30

    var active = vec.activeIterator.toList
    println(active)

    active = active.sortBy(x => x._2)

    println(active)
  }

  def testNullSparseVector() {
    val v = new SparseVector[Double](Array.ofDim[Int](0), Array.ofDim[Double](0), 1000)
    for (i <- 0 to 10) {
      val d = Rand.randInt(1000).draw
      val value = Rand.randInt(10).draw

      println(d + ": " + value)

      v(d) = value
    }

    println(v)

    for (p <- v.activeIterator) {
      println(p)
    }
  }

  def testDenseMatrix() {
    val matrix = DenseMatrix.zeros[Double](5, 5)
    for (i <- 0 until 5; j <- 0 until 5) {
      matrix.update(i, j, i + j + i)
    }

    for (i <- 0 until 5) {
      val dv = matrix(i, ::)
      print(i + " == ")
      println(dv)
    }
  }

  def optimize() {
    val D = 1000

    var vec = new DenseVector(Array.fill(D)(Rand.uniform.draw))
    var data = new DenseVector(Array.fill(D)(Rand.uniform.draw))
    data = data / data.sum
    vec = vec / vec.sum * 1500.0
    println("data = " + data)
    println("initial position = " + vec)

    val func = new DiriOptimizable(data, vec)
    println("initial likelihood = " + func.getValue)

    try {
      val optimizer = new LimitedMemoryBFGS(func)
      optimizer.optimize(1000)
    } catch {
      case x: Exception => println(x.getMessage())
    }

    val result = Array.ofDim[Double](D)
    func.getParameters(result)

    println("result = " + new DenseVector(result))
    println("sum = " + result.sum)
    println("final likelihood = " + func.getValue)
  }

}

class DiriOptimizable(val data: DenseVector[Double], start: Array[Double]) extends Optimizable.ByGradientValue {

  def this(data: DenseVector[Double], start: DenseVector[Double]) = this(data, start.toArray)

  val DELTA = 1E-5
  var x = start.clone
  val dimension = x.length
  val FACTOR = 1500.0

  def getNumParameters() = dimension

  def getParameter(index: Int) = x(index)

  def getParameters(buffer: Array[Double]) {
    for (i <- 0 until dimension) {
      buffer(i) = x(i)
    }
  }

  def setParameter(index: Int, value: Double) {
    x(index) = value

    val sum = x.sum
    x = x.map(_ / sum * FACTOR)
  }

  def setParameters(params: Array[Double]) {
    x = params.clone
    val sum = x.sum
    if (sum != 1)
      x = x.map(_ / sum * FACTOR)
  }

  def getValue(): Double =
    {
      Dirichlet(x).logPdf(data)
    }

  def getValueGradient(buffer: Array[Double]) {
    for (i <- 0 until dimension) {
      var x1 = DenseVector(x.clone)
      x1(i) = x1(i) + DELTA
      x1 = x1 / x1.sum * FACTOR

      var x2 = DenseVector(x.clone)
      x2(i) = x2(i) - DELTA
      x2 = x2 / x2.sum * FACTOR

      val prob1 = Dirichlet(x1).logPdf(data)
      val prob2 = Dirichlet(x2).logPdf(data)

      buffer(i) = (prob1 - prob2) / DELTA / 2
      //println(i + ": " + buffer(i))
    }
  }

}

//class DMTabulizer[V](matrix: Matrix[V])(implicit man: ClassManifest[V]) extends TableWritable[Matrix[V]] {
//  override def header: Option[List[String]] =
//    Some(List("Matrix"))
//
//  override def write(output: TableWriter, value: Matrix[V]) =
//    {
//      for (i <- 0 until matrix.rows) {
//        val row = output.next()
//        for (j <- 0 until matrix.cols) {
//          val cell = row.next()
//          cell.append(matrix(i, j).toString)
//          cell.finish()
//        }
//        row.finish()
//      }
//      output.finish()
//    }
//}