package edu.gatech.eilab.scheherazade.generation.sentselect

import edu.gatech.eilab.scheherazade.data._
import scala.collection.mutable.HashMap

object SentFunctions {

  val probabilityMap: HashMap[String, Double] = null
  val fictionalProbMap: HashMap[String, Double] = null
  val sentimentMap: HashMap[String, Double] = null

  /** probability of a sentence under the bag of word model
   *  
   */
  def BowProbability(sent: Sentence): Double = 0
  def sentiment(sent: Sentence): Double = 0
  def fictionality(sent: Sentence): Double = 0
  
  private def loadProbability() {}
  private def loadFictionality() {}
  private def loadSentiment() {}
}