package edu.gatech.eilab.scheherazade

import io._
import java.io._
import data._

package utils {

  /**
   * find unused sentences, which are sentences that exist
   * in the stories but not included in the gold standard
   *
   */
  object UnusedSentences {

    def main(args: Array[String]) {
      clusterMinusStory
    }

    def clusterMinusStory() {
      val configFile = "configRobExp.txt"
        
      val properties = new SuperProperties()
      val in = new FileInputStream(configFile)
      properties.load(in)
      in.close()
      
      val storyFile = properties.getProperty("storyFile")
      val clusterFile = properties.getProperty("clusterFile")
      
      val stories: List[Story] = SimpleParser.parseStories(storyFile)
      val clusters = SimpleParser.parseClusters(clusterFile)

      val sent1 = stories.flatMap(_.members)
      val sent2 = clusters.flatMap(_.members)

      val unused = sent2.filterNot(sent1 contains)

      println(unused.map(_.toSimpleString()).mkString("\n"))
    }

    def storyMinusCluster() {
      val reader = new ConfigReader("configCoffee.txt")
      val (stories, clusters) = reader.initData

      val sent1 = stories.flatMap(_.members)
      val sent2 = clusters.flatMap(_.members)

      val unused = sent1.filterNot(sent2 contains)

      println(unused.map(_.toSimpleString()).mkString("\n"))
    }
  }
}