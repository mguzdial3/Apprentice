package edu.gatech.eilab.scheherazade.data.serialize

import java.io._
import XStreamable._
import breeze.linalg.DenseMatrix
/**
 * A cached operation for data types that extends the trait XStreamable
 *
 */
object CachedOperation {

  //  protected var cacheFile:File = null
  //  
  //  def setCacheFile(file:File)
  //  {
  //    cacheFile = file
  //  }

  def apply(fn: => DenseMatrix[Double])(cacheFile: File): DenseMatrix[Double] = {
    if (cacheFile.exists()) {
      read[XStreamableMatrix[Double]](cacheFile) match {
        case Some(t) => return t.matrix
        case None =>
      }
    }

    val t = fn
    SevenZip.write(cacheFile, new XStreamableMatrix(t).serialize)
    t
  }

  def apply[T <: XStreamable[T]](fn: => List[T])(cacheFile: File)(implicit m: Manifest[T]): List[T] = {
    if (cacheFile.exists()) {
      read[XStreamableList[T]](cacheFile) match {
        case Some(t) => return t.list
        case None =>
      }
    }

    val t = fn
    SevenZip.write(cacheFile, new XStreamableList(t).serialize)
    t
  }

  /**
   * if the indicated cache file does not exist, performs the computation, and saves the result in the file
   *  Otherwise, reads the result directly from the file.(implicit m: Manifest[T])
   */
  def apply[T <: XStreamable[T]](fn: => T)(cacheFile: File)(implicit m: Manifest[T]): T = {
    if (cacheFile.exists()) {
      read[T](cacheFile) match {
        case Some(t) => return t
        case None =>
      }
    }

    val t: T = fn
//    if (!cacheFile.getName.endsWith(".lzma"))
//    {
//      println(cacheFile.getCanonicalPath())
//      System.exit(1)
//    }
    SevenZip.write(cacheFile, t.serialize)
    t
  }

  private def read[T <: XStreamable[T]](file: File): Option[T] =
    {
      try {

        val string: String =
          {
            //val fileName = file.getName()
            SevenZip.read(file)
//            if (file.getName().endsWith(".lzma")) {
//              // 7zip format data
//              SevenZip.read(file)
//            } else if (file.getName().endsWith(".txt")) {
//              // plain text file
//              scala.io.Source.fromFile(file).mkString
//            } else throw new IOException("Unrecognized file type when reading " + fileName)
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
}