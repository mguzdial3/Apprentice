package edu.gatech.eilab.scheherazade

import data._
import io._
import main._

package cluster.algo {

  object TopicModel {

    def main(args: Array[String]) {
      Global.switchDataSet("Restaurant")
      val reader = new ConfigReader(Global.configFile)
      var (stories, gold) = reader.initData()
      
    }
  }
}