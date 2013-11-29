package edu.gatech.eilab.scheherazade

import nlp.NLPMain
import data._
import data.serialize._
import main._
import io._
import breeze.linalg._

package cluster.ngram {

  object NGTest {
    def main(args: Array[String]) {
      val ngm = new NGramMemory()
      ngm.init()
    }
  }

  abstract class NGramStore() {
    def ngramExists(ngrams: List[Token]): Boolean
    def init()
    def apply(key: String): SparseVector[Double]
    def textExists(key: String): Boolean
  }

  /**
   * stores the ngram data in memory and reads from them directly
   *
   */
  class NGramMemory extends NGramStore with XStreamable[NGramMemory] {
    import java.util.zip.ZipEntry
    import java.util.zip.GZIPInputStream
    import java.io._
    import scala.collection.mutable.HashMap

    protected var store: HashMap[String, SparseVector[Double]] = null

    override def ngramExists(ngrams: List[Token]): Boolean =
      {
        val string = ngrams.map(_.word).mkString(" ")
        store.contains(string)
      }

    override def textExists(key: String): Boolean =
      {
        store.contains(key)
      }

    override def apply(key: String) =
      {
        //        if (store.contains(key)) {
        store(key)
        //        } else {
        //          SparseVector.zeros[Double](1000)
        //        }

      }

    override def init() {
      print("loading distributional ngram clusters ... ")
      val time = System.currentTimeMillis()
      store = loadFiles()

      val seconds = (System.currentTimeMillis() - time) / 1000.0
      println("[" + seconds + " sec]")
    }

    /**
     * load the column vectors of the huge matrix of ngram data
     *
     */
    def loadSparseMatrix(): Array[SparseVector[Double]] =
      {
        val totalNgrams = 9901505 // number computed from corpus
        val map = scala.collection.mutable.HashMap[String, SparseVector[Double]]()

        // length of the vector for each ngram
        val VECTOR_LENGTH = 20

        // file prefix:
        val prefix = "../../phraseClusters/phraseClusters."
        val suffix = ".txt.gz"

        // 1000 column vectors
        //val vectorIdx = Array.fill(1000)(List[Int]())
        //val vectorValues = Array.fill(1000)(List[Double]())
        val vectors = Array.fill(1000)(new SparseVector[Double](Array.ofDim[Int](0), Array.ofDim[Double](0), totalNgrams))

        var ngram = 0 // number of ngrams

        for (i <- 1 to 10) {
          val zipFile = prefix + i + suffix
          println("\nprocessing: " + zipFile)
          val zis =
            new GZIPInputStream(new FileInputStream(zipFile))

          val scanner = new java.util.Scanner(zis)

          while (scanner.hasNext()) {
            val line = scanner.nextLine

            val array = line.split("\t")
            val name = array(0)

            val length = math.min(VECTOR_LENGTH, array.length / 2)

            //val vector = DenseVector.zeros[Double](1000)
            for (i <- 0 until length) {
              val dimension = array(i * 2 + 1).toInt
              val value = array(i * 2 + 2).toDouble

              vectors(dimension)(ngram) = value
            }

            ngram = ngram + 1
            //print(".")
          }

        }
        println("total ngrams = " + ngram)
        vectors
      }

    protected def loadFiles(): HashMap[String, SparseVector[Double]] = {
      val map = scala.collection.mutable.HashMap[String, SparseVector[Double]]()

      // length of the vector for each ngram
      val VECTOR_LENGTH = 20

      // file prefix:
      val prefix = "../../phraseClusters/phraseClusters."
      val suffix = ".txt.gz"

      for (i <- 1 to 10) {
        val zipFile = prefix + i + suffix
        println("processing: " + zipFile)
        val zis =
          new GZIPInputStream(new FileInputStream(zipFile))

        val scanner = new java.util.Scanner(zis)
        while (scanner.hasNext()) {
          val line = scanner.nextLine
          //          println(line)
          val array = line.split("\t")
          val name = array(0)
          val indices = Array.ofDim[Int](VECTOR_LENGTH)
          val values = Array.ofDim[Double](VECTOR_LENGTH)

          val length = math.min(VECTOR_LENGTH, array.length / 2)

          //val vector = DenseVector.zeros[Double](1000)
          for (i <- 0 until length) {
            indices(i) = array(i * 2 + 1).toInt
            values(i) = array(i * 2 + 2).toDouble
            //vector(index) = value
          }

          val vector = SparseVector(1000)((indices zip values): _*)
          val sum = vector.sum
          if (sum != 0) {
            map += ((name -> vector))
          }
          //else println(name + " " + vector )
        }
        print(".")
        //        val av = map.head
        //        val bv = map.tail.head
        //        println(av)
        //        println(bv)
        //
        //        val v1 = av._2
        //        val v2 = bv._2
        //
        //        val sim = (v1 dot v2) / scala.math.sqrt((v1 dot v1) * (v2 dot v2))
        //        println("similarity = " + sim)
      }

      map
    }
  }

  /**
   * reads the ngram data from a SQL database. Currently using MySQL
   *
   */
  class NGramDatabase extends NGramStore {

    import java.util.StringTokenizer
    import java.sql.Connection;
    import java.sql.DriverManager;
    import java.sql.ResultSet;
    import java.sql.SQLException;
    import java.sql.Statement;
    import java.sql.PreparedStatement

    var pst: PreparedStatement = null
    var con: Connection = null

    override def init() {
      createReadConnection()
    }

    override def textExists(key: String): Boolean =
      {
        throw new UnsupportedOperationException()
      }
    //    def tokenize(str: String) =
    //      {
    //        val t = str.trim
    //        val tok = new StringTokenizer(t)
    //        val length = t.count(_ == ' ') + 1
    //        val array = new Array[String](length)
    //
    //        for (i <- 0 until length) {
    //          array(i) = tok.nextToken()
    //        }
    //        array
    //      }

    override def ngramExists(ngrams: List[Token]): Boolean = {

      val word = ngrams.map(_.word).mkString(" ")
      pst.setString(1, word)
      val result = pst.executeQuery()

      var list: List[(Int, Double)] = Nil

      if (result.next) {
        //println(word + " exists")
        true
      } else {
        false
      }

    }

    /**
     * TODO: write a real function
     *
     */
    override def apply(key: String) =
      {
        throw new UnsupportedOperationException()
      }

    protected def createReadConnection() {
      val url = "jdbc:mysql://localhost:3306/phrasal";
      val user = "phrasalUser";
      val password = "123ewq";

      con = DriverManager.getConnection(url, user, password)

      pst = con.prepareStatement("select word from ngram where word = ?")

      val st = con.createStatement()
      st.execute("SET SESSION wait_timeout = 120;")
      st.close
    }
  }
}