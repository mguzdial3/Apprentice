package edu.gatech.eilab.scheherazade.nlp.googlecrawler

import spray.json._
import spray.json.DefaultJsonProtocol._
import scalaj.http._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import java.io._
import NGItemProtocol.listFormat

case class NGItem(ngram: String, timeseries: List[Double]) {
  def averageFreq() =
    {
      val n = timeseries.length
      timeseries.sum / n
    }
}

object NGItemProtocol extends DefaultJsonProtocol {
  implicit val ngitemFormat = jsonFormat2(NGItem)
}

object GoogleQuery {
  val ENGLISH = "eng_2012"
  val ENGLISH_FICTION = "eng_fiction_2012"
  val corpusID = new HashMap[String, Int]() ++ List((ENGLISH, 15), (ENGLISH_FICTION, 16))

  def main(args: Array[String]) {
    val words = List("talk", "walk")
    val l = batchQuery(words)
    println(l)
  }

  /** parse the json returned by Google
   *  
   */ 
  def parseData(json: String): List[NGItem] =
    {
      import NGItemProtocol._
      val jsonAst = json.asJson
      val list = jsonAst.convertTo[List[NGItem]](listFormat[NGItem])

      list
    }

  def singleQuery(word: String, corpus: String = ENGLISH): Double =
    {
      val list = batchQuery(List(word), corpus)
      list.head._2
    }

  def batchQuery(word: List[String], corpus: String = ENGLISH): List[(String, Double)] =
    {
      // this is where we store the final result
      val rtnList = ListBuffer[(String, Double)]()
      // words that are found in the Google ngram corpus
      var returnedWords = List[String]()

      // words that we are looking for in the Google ngram corpus
      val queryWords = word.map(x => x + ":" + corpus)

      // separate words using comma
      val query = queryWords.mkString(",")

      val id = corpusID(corpus).toString
      val request = Http("https://books.google.com/ngrams/graph").params(("content", query), ("year_start", "1991"),
        ("year_end", "2000"), ("corpus", id), ("smoothing", "0"), ("share", "") //, ("direct_url", "t1;," + query + ";,c0")
        ).option(HttpOptions.readTimeout(10000))

      var average = -1.0

      while (average < 0) {
        try {
          val str = request.asString
          //println("http response = " + str)
          val scanner = new java.util.Scanner(str)

          //var found = false
          val numbers = Array.ofDim[Double](10)
          var dataLine: String = null

          while (scanner.hasNextLine() && dataLine == null) {
            val line = scanner.nextLine().trim
            if (line.startsWith("var data")) {
              //println(line)
              val ind = line.indexOf("=")
              dataLine = line.substring(ind + 1, line.length - 1)
              //println(dataLine)
              average = 0
            }
          }

          var nglist = List[NGItem]()
          if (dataLine != null) {
            nglist = parseData(dataLine)
            returnedWords = nglist.map(_.ngram)
          }

          println("Queried Google Ngram (" + corpus + "): " + query)

          // save everything into the rtnList

          for (item <- nglist) {
            rtnList += ((item.ngram, item.averageFreq))
          }
          queryWords.filterNot(returnedWords contains).foreach {
            missingWord =>
              rtnList += ((missingWord, 1e-10))
          }

        } catch {
          case e: IOException =>
            val msg = e.getMessage()
            println("Exception: " + msg)
            Crawler.takeALongBreak()
          //              if (msg.startsWith("Server redirected too many") || msg.startsWith("429: Too Many Requests")) {
          //            	  takeALongBreak()
          //              }
        }
      }

      rtnList.toList

    }

}


