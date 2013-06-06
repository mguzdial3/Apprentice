package edu.gatech.eilab.scheherazade

import data.Sentence
import data.Token
import data.serialize.XStreamable

import java.util.StringTokenizer
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.PreparedStatement

package cluster.ngram {

  class NGramData( //protected[NGramData] var text: String, 
    val sentence: Sentence,
    protected[NGramData] var ngrams: List[List[Token]],
    protected[NGramData] var used: Int,
    val tokens: Array[Token]) extends Ordered[NGramData] with XStreamable[NGramData] {

    def this(sent: Sentence) = this(sent, List[List[Token]](), 0, sent.tokens)

    def remaining() = tokens.length - used
    
    def isComplete() = remaining == 0

    def compare(that: NGramData): Int = {
      /*if (this.remaining > that.remaining) -1
    else if (this.remaining < that.remaining) 1
    else */
      if (NGramData.this.cost < that.cost) 1
      else if (NGramData.this.cost > that.cost) -1
      else 0
    }

    def cost = {
      var badness = ngrams.length + 0.2 * remaining
      for(ng <- ngrams)
      {
        if (ng.exists(token => token.pos.startsWith("N")) && ng.exists(token => token.pos.startsWith("V")))
        {
          // this ngram contains both verbs and nouns. This is a bad ngram. 
          // A good ngram should contain only nouns or only verbs
          badness += 0.3 // A tentative value. Should be less than one. A bad ngram is better than unigrams
        }
      }
      
      badness
    }

    override def equals(any: Any): Boolean =
      any match {
        case that: NGramData =>
          NGramData.this.sentence == that.sentence && NGramData.this.ngrams == that.ngrams
        case _ => false
      }

    override def hashCode() =
      {
        (NGramData.this.ngrams.hashCode * 23 + this.sentence.hashCode() * 31) / 17
      }

    def solutionString() = {
    	//println(ngrams)
      //ngrams.map(_.map(t => t.word + "/" + t.pos).mkString(" ")).mkString(", ")
      ngrams.map(_.map(t => t.word).mkString(" ")).mkString(", ")
    }

    def complete() = (used == tokens.length)

    override def clone(): NGramData = {
      new NGramData(this.sentence, this.ngrams, this.used, this.tokens)
    }

    def clone(newNGrams: List[List[Token]], newUsed: Int) =
      {
        new NGramData(this.sentence, newNGrams, newUsed, this.tokens)
      }

    def nextSplit: List[NGramData] = {
      var list = List[NGramData]()
      val limit = math.min(5, tokens.length - used)

      for (i <- 0 until limit) yield {
        var ngram = List(tokens(used))
        for (j <- (used + 1) to (used + i)) {
          ngram = ngram ::: List(tokens(j))
        }
        //println("testing ngram = " + ngram)
        //if ((NGramData.ngramExists(ngram) && !(ngram.startsWith("John ")) && !(ngram.startsWith("Sally "))) || i == 0) {
        if (i == 0 || NGramData.ngramExists(ngram) ) {
          // when i == 0, this is a single word, and we always accept single words
          // when i > 0, we only accept it if the ngram exists in our database AND it does not start with John or Sally
          val newUsed = i + used + 1
          val newNGrams = ngram :: ngrams
          val split = clone(newNGrams, newUsed)
          list = split :: list
        }
      }
      list
    }

  }

  object NGramData {

    def NGramDropOne(sentence: Sentence) =
      {
        val n = sentence.tokens.length
        val toks = Array.ofDim[Token](n - 1)
        for (i <- 0 until n - 1) {
          toks(i) = sentence.tokens(i)
        }

        new NGramData(sentence, List[List[Token]](), 0, toks)
      }

    var pst: PreparedStatement = null
    var con: Connection = null

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

    def ngramExists(ngrams: List[Token]): Boolean = {

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

    def createReadConnection() = {
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