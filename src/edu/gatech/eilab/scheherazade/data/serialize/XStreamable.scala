package edu.gatech.eilab.scheherazade.data.serialize

import sevenzip.{ SevenZip => SZip }
import java.io._

/**
 * reads and writes strings in the 7zip-compressed format
 *
 */
object SevenZip {

  //lazy val z7: SZip = new SZip()

  /**
   * reads a string from a file in the 7zip format
   *
   */
  def read(file: File): String = {
    val byteStream = new FileInputStream(file)
    // println("reading lzma stream from : " + file.getName())
    val length = byteStream.available()
    val b = new Array[Byte](length)
    byteStream.read(b)
    byteStream.close()
    val string = new SZip().decode(b)
    string
  }

  /*
  private def read7z(filename: String): String = {
    val byteStream = new FileInputStream(filename)
    val length = byteStream.available()
    val b = new Array[Byte](length)
    byteStream.read(b)
    byteStream.close()
    val string = SevenZip.decode(b)
    string
  }*/

  /**
   * writes a string to a file after compressing it in the 7zip format
   *
   */
  def write(file: File, text: String) {
    val out = new BufferedOutputStream(new FileOutputStream(file))
    val bytes = new SZip().encode(text)
    //    println(bytes.mkString(" "))
    // println("writing to " + filename)
    out.write(bytes)
    out.close()
  }
}

/**
 * The XStreamable trait is used to serialize Scala objects.
 *  It seems that java.io.Serializable does not work very well with Scala lists
 */
trait XStreamable[T] {
  //data.XStream.alias(alias(), this.getClass);
  def alias(): String = this.getClass.getSimpleName().toLowerCase()
  def serialize(): String = XStream.toXML(this)
  //def deserialize(xml: String): T = XStream.fromXML(xml).asInstanceOf[T]

}

object XStreamable {

  /**
   * the deserialize method must be put in this object
   *  Unless reflection is used (which is very complicated), I don't know how to put this method into the XStreamable trait
   */
  def deserialize[T <: XStreamable[T]](xml: String): T = XStream.fromXML(xml).asInstanceOf[T]
  
  class XStreamableList[T](val list: List[T]) extends XStreamable[XStreamableList[T]] 
  
  class XStreamableMatrix[T](val matrix: breeze.linalg.DenseMatrix[T]) extends XStreamable[XStreamableMatrix[T]] 
  
  implicit def convertFromList[T](list: List[T])(implicit m: Manifest[T]) = new XStreamableList[T](list)
  implicit def convertBackToList[T](xlist: XStreamableList[T])(implicit m: Manifest[T]) = xlist.list
  
  implicit def convertFromMatrix[T](matrix: breeze.linalg.DenseMatrix[T])(implicit m: Manifest[T]) = new XStreamableMatrix[T](matrix)
  implicit def convertBackToMatrix[T](xmatrix: XStreamableMatrix[T])(implicit m: Manifest[T]) = xmatrix.matrix
}


object XXTest{
  
  import XStreamable._
  
  def main(args: Array[String]) {

    val a = List(1, 2,3, 4)
    val text = a.serialize()
    println(text)
    //    class XInt(val int:Int) extends XStreamable[XInt]
    //    
    //    def fun(i:Int) = {
    //      println("computing....")
    //      new XInt(i * 10 + 2)
    //    }
    //    
    //    val file = new File("testCached.lzma")
    //    file.delete()
    //    val answer = CachedOperation.compute(fun(4), file)
    //    println("answer = " + answer.int)
    //    file.delete()
  }
}
