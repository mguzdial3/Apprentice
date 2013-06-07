package edu.gatech.eilab.scheherazade.main

object Global {

  var configFile = ""
  var parseFile = ""
  var semanticFile = ""
  var locationFile = ""
  var allFile = ""

  /**
   * set the corresponding files according to the data set we are running
   *
   */
  def switchDataSet(dataset: String) {
    if (dataset == "Robbery") {
      configFile = "configRob.txt"
      parseFile = "RobParse.txt"
      semanticFile = "RobSemantic.txt"
      locationFile = "RobLocation.txt"
      allFile = "RobSimilarity.txt"
    } else if (dataset == "Movie") {
      configFile = "configNewMv.txt"
      parseFile = "MvParse.txt"
      semanticFile = "MvSemantic.txt"
      locationFile = "MvLocation.txt"
      allFile = "MvSimilarity.txt"
    } else if (dataset == "Restaurant") {
      configFile = "configRt.txt"
      parseFile = "RtParse.txt"
      semanticFile = "RtSemantic.txt"
      locationFile = "RtLocation.txt"
      allFile = "RtSimilarity.txt"
    } else if (dataset == "Airport") {
      configFile = "configAir.txt"
      parseFile = "AirParse.txt"
      semanticFile = "AirSemantic.txt"
      locationFile = "AirLocation.txt"
      allFile = "AirSimilarity.txt"
    } else if (dataset == "Coffee") {
      configFile = "configCof.txt"
      parseFile = "CofParse.txt"
      semanticFile = "CofSemantic.txt"
      locationFile = "CofLocation.txt"
      allFile = "CofSimilarity.txt"
    }

  }

  def fileSet(dataSet: String) = {

  }
}