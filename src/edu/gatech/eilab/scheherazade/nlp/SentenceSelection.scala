package edu.gatech.eilab.scheherazade

import main._
import io._
import data._
import data.serialize._
import xml._
import javanlp._
import graph._
import similarity._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scalaj.http._
import java.io._

package nlp {

  object SentenceSelection {

    def main(args: Array[String]) {
      computeMaxMin()
    }

    def computeMaxMin() {
      val dataset = "Robbery"
      Global.switchDataSet(dataset)

      val reader = new ConfigReader(Global.configFile)
      var (stories, gold) = reader.initData()

      val text = scala.io.Source.fromFile("Robbery_fictional_words.txt").getLines

      val hashmap = new HashMap[String, Double]() ++ text.map {
        l =>
          val p = l.split(" ")
          p(0).split("_")(0) -> p(1).toDouble
      }

      for (c <- gold) {
        var max = -100000.0
        var maxSent: Sentence = null
        var min = 100000.0
        var minSent: Sentence = null
        println(c.name)
        for (sent <- c.members) {
          var sum =01.0
          for (tok <- sent.tokens) {
            if (hashmap.contains(tok.word)) {
              val v = hashmap(tok.word)
              if (v > 0) {
                sum += math.log(v)
              }
            }
          }

          if (sum > max) {
            max = sum
            maxSent = sent
          }

          if (sum < min) {
            min = sum
            minSent = sent
          }

        }

        println("min sentence: " + minSent.toShortString)
        println("max sentence: " + maxSent.toShortString)
      }
    }

    def loadWordValue() {

    }

    def retrieveData() {
      Global.switchDataSet("Robbery")
      val reader = new ConfigReader(Global.configFile)
      var (stories, gold) = reader.initData()
      def parsed() = CachedOperation {
        SFParser.parse(stories)
      }(Global.parseFile).flatMap(_.members)

      val sents = parsed

      val stopwords = loadStopWords()
      //println(stopwords.mkString("\n"))
      val set = HashSet[String]()
      val pos = HashSet[String]()
      for (c <- gold) {
        //val hashmap = scala.collection.mutable.HashMap[String, Int]()

        for (s <- c.members) {

          val parsedSent = sents.find(x => x.id == s.id).get
          for (token <- parsedSent.tokens) {
            if (!stopwords.contains(token.lemma.toLowerCase())) {
              if (token.pos == "RB" || token.pos.startsWith("N") || token.pos.startsWith("V") || token.pos == "JJ" || token.pos == "RB") {
                val pos = readablePOS(token.pos)
                set += token.word.toLowerCase() + pos
                //pos += token.pos
              }
            }
          }
        }

      }

      val pw = new PrintWriter(new BufferedOutputStream(new FileOutputStream("Robbery_unigram_prob.txt")))

      for (word <- set) {
        println(word)
        var value = -1.0

        do {
          value = query(word)
          val interval = scala.math.random * 5000 + 5000
          Thread.sleep(interval.toInt)
        } while (value < 0)

        pw.print(word)
        pw.print(", ")
        pw.println(value)
        pw.flush()
      }
      pw.close
    }

    def loadStopWords(): HashSet[String] = {
      val text = scala.io.Source.fromFile("stopwords2.txt").getLines

      HashSet[String]() ++= text.map(_.trim)

    }

    def readablePOS(pos: String): String =
      {
        if (pos.startsWith("N")) "_NOUN"
        else if (pos.startsWith("V")) "_VERB"
        else if (pos == "JJ") "_ADJ"
        else if (pos.startsWith("RB")) "_ADV"
        else ""
      }

    def query(word: String): Double =
      {
        val query = word + ":eng_fiction_2012" ///" + word + ":eng_2012"

        try {
          val str = Http("http://books.google.com/ngrams/graph").params(("content", query), ("year_start", "1991"),
            ("year_end", "2000"), ("corpus", "15"), ("smoothing", "0")).option(HttpOptions.readTimeout(10000)).asString

          val scanner = new java.util.Scanner(str)

          var found = false
          val numbers = Array.ofDim[Double](10)

          while (scanner.hasNextLine() && !found) {
            val line = scanner.nextLine().trim
            if (line == "data.addRows(") {
              val dataLine = scanner.nextLine.trim
              val data = dataLine.replaceAll("\\]", "").replaceAll("\\[", "").split(",")
              for (i <- 1 until data.length by 2) {
                numbers((i - 1) / 2) = data(i).trim.toDouble
              }
              println(numbers.mkString(", "))
              found = true
            }
          }

          val average = numbers.sum / 10.0
          average
        } catch {
          case e: Exception =>
            println(e.getMessage())
            -1
        }
      }
  }
}