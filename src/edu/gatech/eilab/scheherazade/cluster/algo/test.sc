package edu.gatech.eilab.scheherazade.cluster.algo

import Jama._

object test {
  val m = Array.fill(4, 5){0.0}                   //> m  : Array[Array[Double]] = Array(Array(0.0, 0.0, 0.0, 0.0, 0.0), Array(0.0,
                                                  //|  0.0, 0.0, 0.0, 0.0), Array(0.0, 0.0, 0.0, 0.0, 0.0), Array(0.0, 0.0, 0.0, 0
                                                  //| .0, 0.0))
  for(i <- 0 until 4; j <- 0 until 5)
  	m(i)(j) = i + j
  val matrix = new Matrix(m)                      //> matrix  : Jama.Matrix = Jama.Matrix@5a9e29fb
  
  matrix.getRowDimension()                        //> res0: Int = 4
  matrix.getColumnDimension()                     //> res1: Int = 5
  
  matrix.getColumnPackedCopy()                    //> res2: Array[Double] = Array(0.0, 1.0, 2.0, 3.0, 1.0, 2.0, 3.0, 4.0, 2.0, 3.0
                                                  //| , 4.0, 5.0, 3.0, 4.0, 5.0, 6.0, 4.0, 5.0, 6.0, 7.0)
  matrix.getRowPackedCopy()                       //> res3: Array[Double] = Array(0.0, 1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0, 4.0, 5.0
                                                  //| , 2.0, 3.0, 4.0, 5.0, 6.0, 3.0, 4.0, 5.0, 6.0, 7.0)
  
  val m2 = matrix.getMatrix(0, 0, 0, 4)           //> m2  : Jama.Matrix = Jama.Matrix@45d64c37
  m2.print(5, 3)                                  //> 
                                                  //|   0.000  1.000  2.000  3.000  4.000
                                                  //| 
	m2.getColumnPackedCopy()                  //> res4: Array[Double] = Array(0.0, 1.0, 2.0, 3.0, 4.0)
}