package edu.gatech.eilab.scheherazade.main
import java.io._
object Global {

  var configFile: File = null
  var parseFile: File = null
  var semanticFile: File = null
  var locationFile: File = null
  var allFile: File = null

  /**
   * set the corresponding files according to the data set we are running
   *
   */
  def switchDataSet(dataset: String) {
    if (dataset == "Robbery") {
      configFile = new File("configRob.txt")
      parseFile = new File("RobParse.txt")
      semanticFile = new File("RobSemantic.txt")
      locationFile = new File("RobLocation.txt")
      allFile = new File("RobSimilarity.txt")
    } else if (dataset == "Movie") {
      configFile = new File("configNewMv.txt")
      parseFile = new File("MvParse.txt")
      semanticFile = new File("MvSemantic.txt")
      locationFile = new File("MvLocation.txt")
      allFile = new File("MvSimilarity.txt")
    } else if (dataset == "Restaurant") {
      configFile = new File("configRt.txt")
      parseFile = new File("RtParse.txt")
      semanticFile = new File("RtSemantic.txt")
      locationFile = new File("RtLocation.txt")
      allFile = new File("RtSimilarity.txt")
    } else if (dataset == "Airport") {
      configFile = new File("configAir.txt")
      parseFile = new File("AirParse.txt")
      semanticFile = new File("AirSemantic.txt")
      locationFile = new File("AirLocation.txt")
      allFile = new File("AirSimilarity.txt")
    } else if (dataset == "Coffee") {
      configFile = new File("configCof.txt")
      parseFile = new File("CofParse.txt")
      semanticFile = new File("CofSemantic.txt")
      locationFile = new File("CofLocation.txt")
      allFile = new File("CofSimilarity.txt")
    }

  }

  def fileSet(dataSet: String) = {

  }
}