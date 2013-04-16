package edu.gatech.eilab.scheherazade.similarity

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.PreparedStatement
import java.io.IOException
import java.util.Properties;

object DistributionalSim extends SimilarityMeasure {
  var pst: PreparedStatement = null
  var con: Connection = null

  def similarity(word1: String, word2: String): Double = {
    if (con == null) createReadConnection()

    try {
      val ng1 = search(word1)
      val ng2 = search(word2)
      ng1.cosineSimilarity(ng2)
    } catch {
      case ne: NotFoundException => 0
    }
  }

  def hasWord(word: String): Boolean =
    {
      if (con == null) createReadConnection()
      try {
        pst.setString(1, word.trim)
        val result = pst.executeQuery()
        result.next
      }
      catch {
        case e:Exception =>
          println(e.getMessage())
          false
      }
    }

  protected def search(word: String): NGram =
    {
      val w = word.trim
      pst.setString(1, w)
      val result = pst.executeQuery()

      var list: List[(Int, Double)] = Nil

      while (result.next) {
        val d = result.getInt(2)
        val v = result.getDouble(3)
        list = (d, v) :: list
      }
      if (list == Nil) throw new NotFoundException()
      NGram(w, list.reverse)
    }

  protected def createReadConnection() = {
    val url = "jdbc:mysql://localhost:3306/phrasal";
    val user = "phrasalUser";
    val password = "123ewq";

    con = DriverManager.getConnection(url, user, password)

    pst = con.prepareStatement("select word, dimension, val from ngram join dimension on id = idngram where word = ?")

    val st = con.createStatement()
    st.execute("SET SESSION wait_timeout = 120;")
    st.close
  }
}

class NotFoundException extends Exception("not found")

case class NGram(val phrase: String, val membership: List[(Int, Double)]) {

  override def equals(obj: Any) = obj match {
    case that: NGram =>
      this.phrase == that.phrase
    case _ => false
  }

  override def hashCode = this.phrase.hashCode()

  //override def toString() = "NGram(" + phrase + membership.mkString("membership = ", ", ", ")")

  def details(): String = toString() + " length = " + sum()

  def sum(): Double =
    {
      membership.map(_._2).sum
    }

  def norm(): Double =
    {
      val s = membership.map(_._2).map(x => math.pow(x, 2)).sum
      math.sqrt(s)
    }

  def cosineSimilarity(that: NGram): Double = {
    var sum = 0.0

    var thisMem = membership.sortWith((x, y) => x._1 < y._1)
    var thatMem = that.membership.sortWith((x, y) => x._1 < y._1)

    var thisHead: (Int, Double) = null
    var thatHead: (Int, Double) = null

    while (thisMem != Nil && thatMem != Nil) {
      thisHead = thisMem.head
      thatHead = thatMem.head
      var thisIndex = thisHead._1
      var thatIndex = thatHead._1

      if (thisIndex == thatIndex) {
        sum += thisHead._2 * thatHead._2
        // point to the next element for both lists
        //println("common dimension: " + thisIndex)
        thisMem = thisMem.tail
        thatMem = thatMem.tail
      } else if (thisIndex < thatIndex) {
        // move this list forward
        thisMem = thisMem.tail
      } else {
        // move that list forward
        thatMem = thatMem.tail
      }

    }

    sum / this.norm() / that.norm()
  }

  def distance(that: NGram): Double = {
    var sum = 0.0

    var thisMem = membership.sortWith((x, y) => x._1 < y._1)
    var thatMem = that.membership.sortWith((x, y) => x._1 < y._1)

    var thisHead: (Int, Double) = null
    var thatHead: (Int, Double) = null

    while (thisMem != Nil && thatMem != Nil) {
      thisHead = thisMem.head
      thatHead = thatMem.head
      var thisIndex = thisHead._1
      var thatIndex = thatHead._1

      if (thisIndex == thatIndex) {
        sum += math.pow(thisHead._2 - thatHead._2, 2)
        // point to the next element for both lists
        //println("common dimension: " + thisIndex)
        thisMem = thisMem.tail
        thatMem = thatMem.tail
      } else if (thisIndex < thatIndex) {
        sum += math.pow(thisHead._2, 2)
        // move this list forward
        thisMem = thisMem.tail
      } else {
        sum += math.pow(thatHead._2, 2)
        // move that list forward
        thatMem = thatMem.tail
      }

    }

    while (thisMem != Nil) {
      thisHead = thisMem.head
      thisMem = thisMem.tail
      sum += math.pow(thisHead._2, 2)
    }

    while (thatMem != Nil) {
      thatHead = thatMem.head
      thatMem = thatMem.tail
      sum += math.pow(thatHead._2, 2)
    }

    math.sqrt(sum)
  }
}

  