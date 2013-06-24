package edu.gatech.eilab.scheherazade.temp

abstract class SimpleOp[T] {

  def compute():T
}

trait ComplexOp[T] extends SimpleOp[T]
{
	abstract override def compute():T = {
	  println("A more complex computation")
	  super.compute()
	}
}

class RealOp extends SimpleOp[Int] {
  
  def compute():Int = 
  {
    println("Real Op being computed")
    5
  }
}

//object MMMain {
//  def main(args:Array[String])
//  {
//    
//    val r = new RealOp() with ComplexOp[Double]
//    val result = r.compute()
//    println("result = " + result)
//  }
//}