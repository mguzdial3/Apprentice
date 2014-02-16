package edu.gatech.eilab.scheherazade.nlp

class StopWordStore {

  private val stopwords = loadStopWords()
  
  private def loadStopWords():List[String] = 
  {
    val lines = scala.io.Source.fromFile("stopwords2.txt").getLines
    lines.map {
      line => line.trim.toLowerCase()     
    }.toList
  }
  
  def isStopWord(word:String) = stopwords contains (word.toLowerCase())  
}

