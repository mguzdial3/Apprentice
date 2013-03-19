package edu.gatech.eilab.scheherazade.cluster

import breeze.linalg._

object spectral {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(105); 
  
  val n = 5;System.out.println("""n  : Int = """ + $show(n ));$skip(97); 
  
  val dm = DenseMatrix.fill[Double](n, n){
  val r = math.random
  if (r > 0.3) r else 0
  };System.out.println("""dm  : breeze.linalg.DenseMatrix[Double] = """ + $show(dm ));$skip(39); 
  
  val diag = DenseVector.fill(n)(0);System.out.println("""diag  : breeze.linalg.DenseVector[Int] = """ + $show(diag ));$skip(13); val res$0 = 
  
  dm(1,1);System.out.println("""res0: Double = """ + $show(res$0));$skip(110); 
  
  for(i <- 0 until n)
  {
  	var sum = 0.0
  	for (j <- 0 until n)
  		sum += dm(i,j)
  	diag(i) = sum
  }}
  

}