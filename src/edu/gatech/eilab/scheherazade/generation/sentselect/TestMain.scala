package edu.gatech.eilab.scheherazade.generation.sentselect
import edu.gatech.eilab.scheherazade.io._
import edu.gatech.eilab.scheherazade.nlp._

object TestMain {

  def main(args: Array[String]) {
    val reader = new ConfigReader("configRobBest.txt")
    var (stories, clusters) = reader.initDataFiltered()

    for (sent <- clusters(0).members) {
      println(sent)

    }

    clusters = SFParser.parse(clusters)

    for (sent <- clusters(0).members) {
      println(sent)

    }
  }
}