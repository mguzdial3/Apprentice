package edu.gatech.eilab.scheherazade.generation.sentselect
import edu.gatech.eilab.scheherazade.io._
import edu.gatech.eilab.scheherazade.nlp._

object TestMain {

  def main(args: Array[String]) {
    val sent = SFParser.parse("John approached the bank door, which was a revolving door, and put his hands on the glass to turn the door.")

    println(sent)
    val prob = UniGramModel.logProbability(sent)
    println(prob)
  }
}