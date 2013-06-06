package edu.gatech.eilab.scheherazade.data.serialize

import sevenzip.{ SevenZip => SZip }
import java.io._

/** reads and writes strings in the 7zip-compressed format
 *  
 */ 
object SevenZip {

  //lazy val z7: SZip = new SZip()

  /** reads a string from a file in the 7zip format
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

  /** writes a string to a file after compressing it in the 7zip format
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

/** The XStreamable trait is used to serialize Scala objects. 
 *  It seems that java.io.Serializable does not work very well with Scala lists
 */
trait XStreamable[T] {
  //data.XStream.alias(alias(), this.getClass);
  def alias(): String = this.getClass.getSimpleName().toLowerCase()
  def serialize(): String = XStream.toXML(this)
  //def deserialize(xml: String): T = XStream.fromXML(xml).asInstanceOf[T]

}

object XStreamable {

  /** the deserialize method must be put in this object
   *  Unless reflection is used (which is very complicated), I don't know how to put this method into the XStreamable trait
   */
  def deserialize[T <: XStreamable[T]](xml: String): T = XStream.fromXML(xml).asInstanceOf[T]
}

/** A cached operation for data types that extends the trait XStreamable
 *  
 */
object CachedOperation {
  import java.io._

  /** if the indicated cache file does not exist, performs the computation, and saves the result in the file
   *  Otherwise, reads the result directly from the file.(implicit m: Manifest[T])
   */ 
  def compute[T <: XStreamable[T]](fn: => T, file: File):T = {
    if (file.exists()) {
    	read[T](file) match {
    	  case Some(t) => return t
    	  case None =>
    	}
    }
    
    val t:T = fn
    SevenZip.write(file, t.serialize)
    t
  }

  private def read[T <: XStreamable[T]](file: File): Option[T] =
    {
      try {

        val string: String =
          {
            val fileName = file.getName()
            if (file.getName().endsWith(".lzma")) {
              // 7zip format data
              SevenZip.read(file)
            } else if (file.getName().endsWith(".txt")) {
              // plain text file
              scala.io.Source.fromFile(file).mkString
            } else throw new IOException("Unrecognized file type when reading " + fileName)
          }

        val obj = XStreamable.deserialize[T](string)
        return Some(obj)

      } catch {
        case ex: IOException =>
          println("XML Reading Error: " + ex.getClass().getName() + " " + ex.getMessage())
          ex.printStackTrace()
          None
      }
    }
  
//  def main(args:Array[String]){
//    
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
//  }

}