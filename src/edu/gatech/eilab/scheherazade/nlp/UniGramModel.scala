package edu.gatech.eilab.scheherazade.nlp

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import java.io._
import edu.gatech.eilab.scheherazade.utils.MathUtils._
import edu.gatech.eilab.scheherazade.data._

object UniGramModel {

  private val probabilityMap = loadMap("unigram_prob.txt")
  private val fictionalProbMap = loadMap("unigram_prob_fiction.txt")

  private val nonexistent = ListBuffer[String]()

  private def loadMap(filename: String) =
    {
      val map = HashMap[String, Double]()
      val lines = scala.io.Source.fromFile(new File(filename)).getLines
      for (line <- lines) {
        val split = line.split(" ")
        val word = split(0)
        val value = split(1).toDouble
        map += (word -> value)
      }

      map
    }

  def logProbability(sent: SingleDescription) =
    {
      val tokens = usefulTokenKeys(sent.allTokens)
      val countMap = tokens.groupBy(x => x).map(pair => (pair._1 -> pair._2.size))
      //println(countMap)
      val counts = countMap.map(_._2)
      val total = counts.sum

      val coeff = logFactorial(total) - counts.map(logFactorial).sum

      var prob = 0.0
      for (wordCount <- countMap) {
        val word = wordCount._1
        val c = wordCount._2
        prob += math.log(probabilityMap.getOrElse(word, 1e-10)) * c
        if (!probabilityMap.contains(word)) {
          println("not found word: " + word)
          nonexistent += word
        } else {
          //println(word + ": " + math.log(probabilityMap(word)) + " * " + c)
        }
      }

      prob
    }

  private def usefulTokenKeys(tokens: List[Token]): List[String] =
    {
      tokens.filter(x => isUsefulPOS(x.pos) && (!StopWordStore.isStopWord(x.lemma))).map(x => x.lemma + toGooglePOS(x.pos))
    }

  def fictionality(sent: SingleDescription): List[Double] =
    {
      val tokens = usefulTokenKeys(sent.allTokens)
      val fictionality = tokens map {
        key =>
          if (probabilityMap.contains(key) && fictionalProbMap.contains(key))
          {
            fictionalProbMap(key) / probabilityMap(key) 
          }
          else
          {
            System.err.println("word " + key + " does not exist in our knowledge base")
            0.0
          }
      }
      
      fictionality
    }

  def printNon() {
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("nonexistent.txt")))
    nonexistent.distinct foreach {
      word =>
        pw.println(word)
    }
    pw.close
  }

  private def isUsefulPOS(pos: String): Boolean =
    {
      pos.startsWith("NN") || pos.startsWith("VB") || pos.startsWith("JJ") || pos.startsWith("RB")
    }

  /**
   * return the google POS in the form of "_NOUN" or "_VERB"
   *
   */
  private def toGooglePOS(pos: String): String =
    {
      if (pos.startsWith("N")) "_NOUN"
      else if (pos.startsWith("V")) "_VERB"
      else if (pos.startsWith("JJ")) "_ADJ"
      else if (pos.startsWith("RB")) "_ADV"
      else {
        System.err.println("WARNING: WHAT POS IS THIS ? " + pos)
        ""
      }
    }

}