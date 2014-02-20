package edu.gatech.eilab.scheherazade.generation.sentselect

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.io._
import edu.gatech.eilab.scheherazade.nlp._
import edu.gatech.eilab.scheherazade.utils.MathUtils._
import SentenceSelectionFunctions._
import edu.gatech.eilab.scheherazade.generation._

object TestMain {

  def sentEval(c: ClusterLike): List[(SingleDescription, Double)] =
    reciprocalRank(c, UniGramModel.logProbability).map(x => x._1 -> x._2)

  def adjEval(prev: SingleDescription, cluster: ClusterLike): List[(SingleDescription, Double)] = adjacentHeuristic(prev, cluster)

  def main(args: Array[String]) {

    //val clusters = SimpleParser.parseClusters("./data/new_movie/movieGold-cr.gold")
    val clusters = SimpleParser.parseClusters("./data/robbery/robberyGold2-cr.gold")
    val snipClusters = SFParser.parseSnippets(clusters)

    val story = StoryGenerator.genStory.map{
      e => snipClusters.find(c => c.name == e.name) match {
        case Some(c) => c
        case None => throw new RuntimeException("cannot find cluster " + e.name)
    }}

    val sentSelector = new SentenceSelector(sentEval, adjEval)
    val result = sentSelector.bestSentenceSequence(story)
    println(result.mkString("\n"))

    UniGramModel.printNon
  }

  def test2() {

    val s1 = SFParser.parse(1, "John approached the bank door, which was a revolving door, and put his hands on the glass to turn the door.")
    println(s1)
    val s2 = SFParser.parse(2, "John took a deep breath and opened the bank door, letting an elderly woman exit before he entered himself.")
    println(s2)
    val c1 = new Cluster("open door", List(s1, s2))

    val s3 = SFParser.parse(3, "John slowly walked into the bank trying not to draw attention to himself.")
    println(s3)
    val s4 = SFParser.parse(4, "John opened the door, took a quick look around, and entered the bank.")
    println(s4)
    val c2 = new Cluster("enter bank", List(s3, s4))

    //println(adjEval(s1, c2))
    //println(adjEval(s2, c2))

    val sentSelector = new SentenceSelector(sentEval _, adjEval _)
    val result = sentSelector.bestSentenceSequence(List(c1, c2))
    println(result.mkString("\n"))
  }

  def test1() {
    val sent = SFParser.parse("John approached the bank door, which was a revolving door, and put his hands on the glass to turn the door.")

    println(sent)
    val prob = UniGramModel.logProbability(sent)
    println(prob)
  }
}