package edu.gatech.eilab.scheherazade.generation.sentselect
import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap
import java.io._

class UniGramModel {

  val probabilityMap = loadMap("unigram_prob.txt")
  val fictionalProbMap = loadMap("unigram_prob_fiction.txt")

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

  def probability(sent: Sentence) =
    {
	  
    }

  def isUsefulPOS(pos: String): Boolean =
    {
	  pos.startsWith("NN") || pos.startsWith("VB") || pos.startsWith("JJ") || pos.startsWith("RB")
    }

  /** return the google POS in the form of "_NOUN" or "_VERB"
   *  
   */
  def toGooglePOS(pos: String): String =
    {
      if (pos.startsWith("N")) "_NOUN"
      else if (pos.startsWith("V")) "_VERB"
      else if (pos == "JJ") "_ADJ"
      else if (pos.startsWith("RB")) "_ADV"
      else ""
    }

}