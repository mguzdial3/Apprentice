package edu.gatech.eilab.scheherazade.temp

/**
 * playing with Dirichlet Process
 *
 */

import breeze.linalg._
import breeze.stats.distributions._
import java.io._
import scala.collection.mutable.ArrayBuffer

object DiriProcess {

  val gamma = new Gamma(3, 0.1)
  val beta = new Beta(1, 1)

  def main(args: Array[String]) {
    val data = readSyntheticData()
    println(data.map(_.mkString(", ")).mkString("\n"))
  }
  
  def readSyntheticData():Array[Array[Double]] =
  {
    val data = ArrayBuffer[Array[Double]]()
    val text = scala.io.Source.fromFile("SyntheticDirichlet.txt").getLines
    for (line <- text) {
      if (!line.startsWith("Dirichlet"))
      {
        val array = line.split("\\t").map(_.trim.toDouble)
        data += array
      }
    }
    
    data.toArray
  }
  
  def synthesizeData(){

    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("SyntheticDirichlet.txt")))
    
    val diri = Array.fill(4)(drawDirichlet(5))

    for (i <- 0 until diri.length) {

      pw.print("Dirichlet: \t")
      pw.println(diri(i).params.toArray.mkString("\t"))
      for (j <- 1 to 5) {
        val v = diri(i).draw
        pw.println(v.toArray.mkString("\t"))
      }
    }
    
    pw.close

  }

  def drawDirichlet(dimension: Int) =
    {
      val precision = 1 / gamma.draw
      var alpha = Array.fill(dimension)(beta.draw)
      val sum = alpha.sum
      alpha = alpha.map(_ * precision / sum)

      Dirichlet(alpha)
    }
}