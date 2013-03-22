package edu.gatech.eilab.scheherazade.cluster.algo

import Jama._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(112); 
  val m = Array.fill(4, 5){0.0};System.out.println("""m  : Array[Array[Double]] = """ + $show(m ));$skip(57); 
  for(i <- 0 until 4; j <- 0 until 5)
  	m(i)(j) = i + j;$skip(29); 
  val matrix = new Matrix(m);System.out.println("""matrix  : Jama.Matrix = """ + $show(matrix ));$skip(30); val res$0 = 
  
  matrix.getRowDimension();System.out.println("""res0: Int = """ + $show(res$0));$skip(30); val res$1 = 
  matrix.getColumnDimension();System.out.println("""res1: Int = """ + $show(res$1));$skip(34); val res$2 = 
  
  matrix.getColumnPackedCopy();System.out.println("""res2: Array[Double] = """ + $show(res$2));$skip(28); val res$3 = 
  matrix.getRowPackedCopy();System.out.println("""res3: Array[Double] = """ + $show(res$3));$skip(43); 
  
  val m2 = matrix.getMatrix(0, 0, 0, 4);System.out.println("""m2  : Jama.Matrix = """ + $show(m2 ));$skip(17); 
  m2.print(5, 3);$skip(26); val res$4 = 
	m2.getColumnPackedCopy();System.out.println("""res4: Array[Double] = """ + $show(res$4));$skip(15); 
	
	val a = 1.0;System.out.println("""a  : Double = """ + $show(a ));$skip(14); val res$5 = 
	(a*10).toInt;System.out.println("""res5: Int = """ + $show(res$5))}
	
}