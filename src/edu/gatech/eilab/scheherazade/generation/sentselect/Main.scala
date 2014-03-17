package edu.gatech.eilab.scheherazade.generation.sentselect

import edu.gatech.eilab.scheherazade.data._
import edu.gatech.eilab.scheherazade.io._
import edu.gatech.eilab.scheherazade.nlp._
import edu.gatech.eilab.scheherazade.utils.MathUtils._
import SentenceEvalFunctions._
import edu.gatech.eilab.scheherazade.generation._
import java.io._
import scala.collection.mutable.ListBuffer

object Main {

  def sentEval(c: ClusterLike): List[(SingleDescription, Double)] =
    reciprocalOfRank(c, UniGramModel.logProbability).map(x => x._1 -> x._2 * 3)

  def adjEval(prev: SingleDescription, cluster: ClusterLike): List[(SingleDescription, Double)] = adjacentHeuristic(prev, cluster)

  def main(args: Array[String]) {

    genStory()
  }

  def workForHong() {
    val input = scala.io.Source.fromFile("hong-options.txt").getLines
    val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("hong-sentiments.txt")))

    val list = ListBuffer[(String, SingleDescription)]()
    for (line <- input) {
      val split = line.trim.split(" ")
      if (split.length >= 3) // sentences with less than 3 words are discarded 
      {
        val snippet = SFParser.parseMultiple(line)
        list += ((line, snippet))
      }
    }

    val idf = new InverseSentFreq(list.map(_._2).toList)

    for (pair <- list) {
      val desc = pair._2
      val sentiment = exponentialAverage(UniGramModel.sentiments(desc), 1)
      //pw.println(pair._1 + " : " + sentiment)
    }

    pw.close

    UniGramModel.printNon
  }

  def fakeOutline() =
    {
      val names = List("John drives to bank", "John opens bank door", "John covers face",
        "John enters bank", "John scans the bank", "John sees Sally", "waits in line", "John approaches Sally",
        "Sally scared", "John pulls out gun", "John points gun at Sally", "Sally screams", "John demands money",
        "John gives Sally bag", "Sally puts money in bag", "John collects money", "Sally cries",
        "Sally calls police", "John gets in car", "John drives away")

      val clusters = SimpleParser.parseClusters("./data/robbery/robberyGold2-cr.gold")

      names.map(name => clusters.find(_.name == name).get)

    }

  def genStory() {

    val clusters = SimpleParser.parseClusters("./data/new_movie/movieGold-cr.gold")
    //val clusters = SimpleParser.parseClusters("./data/robbery/robberyGold2-cr.gold")
    val snipClusters = SFParser.parseSnippets(clusters)

    val story = StoryGenerator.genStory.map {
    //val story = fakeOutline().map {
      e =>
        snipClusters.find(c => c.name == e.name) match {
          case Some(c) => c
          case None => throw new RuntimeException("cannot find cluster " + e.name)
        }
    }

    val idf = new InverseSentFreq(snipClusters.flatMap(x => x.members))

    def MIDEval(c: ClusterLike) = harmonicMeanRank(c,
      s => UniGramModel.logProbability(s),
      s => -1 * exponentialAverage(UniGramModel.fictionality(s), 12)).map { p => (p._1, 1.0 / p._2) }

    def rankCS(c: ClusterLike) = harmonicMeanRank(c,
      s => UniGramModel.logProbability(s) * -1,
      s => exponentialAverage(UniGramModel.fictionality(s), 12)).map { p => (p._1, 1.0 / p._2) }

    def mostProbEval(c: ClusterLike) = reciprocalOfRank(c, s => UniGramModel.logProbability(s) * -1)
    def leastProbEval(c: ClusterLike) = reciprocalOfRank(c, s => UniGramModel.logProbability(s))

    def leastWords(c: ClusterLike) = reciprocalOfRank(c, s => s.allTokens.size)

    def MID2Eval(cluster: ClusterLike): List[(SingleDescription, Double)] =
      {
        val rank1 = rank(cluster, s => UniGramModel.logProbability(s))
        val rank2 = rank(cluster, s => -1 * exponentialAverage(UniGramModel.fictionality(s), 12))
        val rank3 = rank(cluster, s => s.allTokens.size)

        val list =
          for (sent <- uniques(cluster.members)) yield {
            val r1 = rank1.find(_._1.toText == sent.toText).get._2
            val r2 = rank2.find(_._1.toText == sent.toText).get._2
            val r3 = rank3.find(_._1.toText == sent.toText).get._2

            //          println("sentence: ***")
            //          println(sent.toText)
            //          println(r1: )
            //          println(sent.toText)
            //          println("******")

            val fitness = 3.0 / ((1.0 / r1) + (1.0 / r2) + (1.0 / r3))
            (sent, fitness)
          }

        val sortedList = list.sortWith(_._2 < _._2)

        var i = 0
        for (item <- sortedList) yield {
          i += 1 
          (item._1, 1.0 / i) // this is a reciprocal
        }
      }

    def ficEval(cl: ClusterLike): List[(SingleDescription, Double)] = {
      val ranks = reciprocalOfRank(cl,
        x => {
          -1 * exponentialAverage(UniGramModel.fictionality(x), 12)
        })

      ranks
    }

    def positiveEval(cl: ClusterLike): List[(SingleDescription, Double)] = {
      val ranks = rank(cl, x => -1 * exponentialAverage(UniGramModel.sentiments(x), 2))
      ranks.map { p => (p._1, 1.0 / p._2) }
    }

    def negativeEval(cl: ClusterLike): List[(SingleDescription, Double)] = {
      val ranks = rank(cl, x => exponentialAverage(UniGramModel.sentiments(x), 6))
      ranks.map { p => (p._1, 1.0 / p._2) }
    }

    snipClusters.foreach {
      c =>
        //        val maxFictional = c.members.maxBy(s => exponentialAverage(UniGramModel.fictionality(s), 3))
        println("*****")
        println(c.name)
        //        println("MF: " + maxFictional.toText)
        //
        //        val minProbable = c.members.minBy(s => UniGramModel.logProbability(s))
        //        println("LP: " + minProbable.toText)

        //for (i <- 1 to 10) {
        //          val positive = c.members.maxBy(s => exponentialAverage(UniGramModel.sentiments(s), 3))
        //          val negative = c.members.minBy(s => exponentialAverage(UniGramModel.sentiments(s), 3))
        //          println("positive: " + positive.toText)
        //          println("negative: " + negative.toText)
        //}

        val mf = ficEval(c).maxBy(p => p._2)._1
        println("MF: " + mf.toText)

        val lp = leastProbEval(c).maxBy(p => p._2)._1
        println("LP: " + lp.toText)

        val mid = MIDEval(c).maxBy(p => p._2)._1
        println("MID: " + mid.toText)
        
        val mid2 = MID2Eval(c).maxBy(p => p._2)._1
        println("MID2: " + mid2.toText)

//        val lf = ficEval(c).minBy(p => p._2)._1
//        println("LF: " + lf.toText)
//
//        val mp = leastProbEval(c).minBy(p => p._2)._1
//        println("MP: " + mp.toText)
//
//        val positive = positiveEval(c).maxBy(p => p._2)._1
//        println("Positive: " + positive.toText)
//
//        val negative = negativeEval(c).maxBy(p => p._2)._1
//        println("Negative: " + negative.toText)
    }

    def combinedEval(cl: ClusterLike): List[(SingleDescription, Double)] = {
      val ranks = harmonicMeanRank(cl,
        x => {
          -1 * exponentialAverage(UniGramModel.fictionality(x), 10)
        },
        x => { UniGramModel.logProbability(x) })

      ranks.map { p => (p._1, (2.0 / p._2)) }
    }

    def adjEvalIdf(prev: SingleDescription, cluster: ClusterLike): List[(SingleDescription, Double)] = adjacentHeuristic(prev, cluster)

    val sentSelector = new SentenceSelector(MID2Eval, adjEval)
    val result = sentSelector.bestSentenceSequence(story)
    println(result.mkString("\n"))
    //UniGramModel.printNon
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
    //    val result = sentSelector.bestSentenceSequence(List(c1, c2))
    //    println(result.mkString("\n"))
  }

  def test1() {
    val sent = SFParser.parse("John approached the bank door, which was a revolving door, and put his hands on the glass to turn the door.")

    println(sent)
    val prob = UniGramModel.logProbability(sent)
    println(prob)
  }
}