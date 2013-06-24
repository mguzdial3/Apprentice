package edu.gatech.eilab.scheherazade.temp

/** playing with the Breeze library
 * 
 */
import breeze.linalg._

//import breeze.io._
//import breeze.io.TextWriter._
import java.io._
object Play {

  def main(args: Array[String]) {

	  var m = DenseMatrix.tabulate[Double](3, 3)((i,j) => (i+1)*(j+1))
	  val factor = 0.85
	  val n = m * factor + (1-factor) * DenseMatrix.fill(3, 3)(1/3.0)
      println(n.toString)
    
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