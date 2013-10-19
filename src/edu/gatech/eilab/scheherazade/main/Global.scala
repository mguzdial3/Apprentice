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
      parseFile = new File("RobParse.lzma")
      semanticFile = new File("RobSemantic.lzma")
      locationFile = new File("RobLocation.lzma")
      allFile = new File("RobSimilarity.lzma")
    } else if (dataset == "Movie") {
      configFile = new File("configNewMv.txt")
      parseFile = new File("MvParse.lzma")
      semanticFile = new File("MvSemantic.lzma")
      locationFile = new File("MvLocation.lzma")
      allFile = new File("MvSimilarity.lzma")
    } else if (dataset == "Restaurant") {
      configFile = new File("configRt.txt")
      parseFile = new File("RtParse.lzma")
      semanticFile = new File("RtSemantic.lzma")
      locationFile = new File("RtLocation.lzma")
      allFile = new File("RtSimilarity.lzma")
    } else if (dataset == "Airport") {
      configFile = new File("configAir.txt")
      parseFile = new File("AirParse.lzma")
      semanticFile = new File("AirSemantic.lzma")
      locationFile = new File("AirLocation.lzma")
      allFile = new File("AirSimilarity.lzma")
    } else if (dataset == "Coffee") {
      configFile = new File("configCof.txt")
      parseFile = new File("CofParse.lzma")
      semanticFile = new File("CofSemantic.lzma")
      locationFile = new File("CofLocation.lzma")
      allFile = new File("CofSimilarity.lzma")
    }

  }

//  def fileSet(dataSet: String) = {
//
//  }
  
  def setConfig(newConfig:File)
  {
    configFile = newConfig
  }
}